import * as wasm from "cpu-emulator-pwa";
import "./elm.js"

/////////////////////////////////// language support for assembly ////////////////////////////////
// Register a new language
monaco.languages.register({ id: 'asm', extensions: [ '.asm' ] });

// Register a tokens provider for the language
monaco.languages.setMonarchTokensProvider('asm', {
  symbols: /[\-\=\+\&\|\!\@]+/,
  keywords: [],
  operators: [
    '=', '-', '+', '&', '|', '!', '@'
  ],
  digits: /\d+(_+\d+)*/,
  binarydigits: /[0-1]+(_+[0-1]+)*/,
  hexdigits: /[[0-9a-fA-F]+(_+[0-9a-fA-F]+)*/,
  tokenizer: {
    root: [
      { include: 'common' }
    ],
    common: [
      // keyword and identifier
      [/[a-zA-Z][a-zA-Z0-9_]*/, {
        cases: {
          '@keywords': 'keyword',
          '@default': 'identifier'
        }
      }],

      // number
      [/0[xX](@hexdigits)n?/, 'number.hex'],
      [/0[bB](@binarydigits)n?/, 'number.binary'],
      [/(@digits)n?/, 'number'],

      // whitespace
      { include: '@whitespace' },

      // delimiters and operators
      [/[()]/, '@brackets'],
      [/!(?=([^=]|$))/, 'delimiter'],
      [/@symbols/, {
        cases: {
          '@operators': 'delimiter',
          '@default': ''
        }
      }],
    ],
    whitespace: [
      [/[ \r\n]+/, ''],
      [/\{\-/, 'comment', '@comment'],
      [/\-\-.*$/, 'comment'],
    ],
    comment: [
      [/[^\{\-]+/, 'comment'],
      [/\-\}/, 'comment', '@pop'],
      [/[\{\-]/, 'comment']
    ],
  }
});
monaco.languages.setLanguageConfiguration('asm', {
  brackets: [
    ['(', ')']
  ],
  comments: {
    lineComment: '--',
    blockComment: ['{-', '-}']
  },
  autoClosingPairs: [
    { open: '(', close: ')' },
  ],
});


/////////////////////////////// initialize monaco editor ///////////////////////////////

var editor = monaco.editor.create(document.getElementById("editor"), {
  theme: "vs",
  value: "",
  language: 'asm'
});

/////////////////////////////////// initialize elm app /////////////////////////////////////////

var localStorageKey = "cpu-emulator-pwa";

// load saved project from localStorage
var savedModelString = localStorage.getItem(localStorageKey);

var app = Elm.Main.init({ node: document.querySelector("main"), flags: savedModelString });

app.ports.saveModelPort.subscribe(function(modelJson) {
  var modelString = JSON.stringify(modelJson);
  localStorage.setItem(localStorageKey, modelString);
});

var screen = document.getElementById("screen").getContext('2d');

wasm.initialize();

editor.onDidChangeModelContent(function(event) {
  app.ports.editProgramInElmPort.send(editor.getValue({ lineEnding : "\n"}));
});

app.ports.editProgramInEditorPort.subscribe(function(newContent) {
  editor.setValue(newContent);
});

app.ports.stepComputerPort.subscribe(function([ramDisplaySize, cycles]) {
  let newComputer = wasm.step(ramDisplaySize, cycles);
  updateScreen(newComputer.updated_pixels);
  app.ports.receiveComputerPort.send(newComputer);
});

app.ports.stepComputeLite.subscribe(function(cycles) {
  let updatedPixels = wasm.step_lite(cycles);
  updateScreen(updatedPixels);
});

app.ports.askForComputerPort.subscribe(function(ramDisplaySize) {
  let newComputer = wasm.ask_for_computer(ramDisplaySize);
  updateScreen(newComputer.updated_pixels);
  app.ports.receiveComputerPort.send(newComputer);
});

app.ports.editRomPort.subscribe(function(values) {
  wasm.edit_rom(values);
});

app.ports.editRamPort.subscribe(function([index, value]) {
  wasm.edit_ram(index, value);
});

app.ports.setRamPort.subscribe(function(values) {
  wasm.set_ram(values);
})

function updateScreen(pixels) {
  pixels.forEach(function({ x, y, color }) {
    screen.fillStyle = color;
    screen.fillRect( x, y, 1, 1 );
  });
}

app.ports.resetComputerPort.subscribe(function(ramDisplaySize) {
  let newComputer = wasm.reset(ramDisplaySize);
  app.ports.receiveComputerPort.send(newComputer);
  screen.clearRect(0, 0, screen.canvas.width, screen.canvas.height);
});

app.ports.scrollIntoViewPort.subscribe(function(ids) {
  ids.forEach(function(id) {
    var element = document.getElementById(id);
    if (element !== null) {
      setTimeout(function() {
        element.scrollIntoView({behavior: "auto", block: "nearest"});
      }, 100);
    }
  });
});

app.ports.showProgramEditorPort.subscribe(function() {
  document.getElementById("editor").style.top = "10px";
  editor.focus();
});

app.ports.hideProgramEditorPort.subscribe(function() {
  document.getElementById("editor").style.top = "-100vh";
});

app.ports.showAssemblerErrorPort.subscribe(function([[row, col], errorMessage]) {
  monaco.editor.setModelMarkers(editor.getModel(), 'Assembler Error', [{
    startLineNumber: row,
    startColumn: col,
    endLineNumber: row,
    endColumn: col + 1,
    message: errorMessage,
    severity: monaco.MarkerSeverity.Error
  }]);
});

app.ports.clearAssemblerErrorPort.subscribe(function() {
  monaco.editor.setModelMarkers(editor.getModel(), 'Assembler Error', []);
});


if ('serviceWorker' in navigator) {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register('./sw.js').then(registration => {
      console.log('SW registered: ', registration);
    }).catch(registrationError => {
      console.log('SW registration failed: ', registrationError);
    });
  });
}
