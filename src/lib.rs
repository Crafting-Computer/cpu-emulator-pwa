mod utils;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use std::convert::TryInto;
extern crate serde_json;
extern crate web_sys;
use std::fmt::Debug;
use gloo_events::EventListener;

#[macro_use]
extern crate serde_derive;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;


const RAM_SIZE: usize = 84737; // 2 ^ 16 + 19200 + 1
const ROM_SIZE: usize = 65536; // 2 ^ 16
const SCREEN: usize = 65536;   // 2 ^ 16
const KEYBOARD: usize = 84736; // 2 ^ 16 + 19200


struct Computer {
    a : i32,
    d : i32,
    m : i32,
    pc : u32,
    ram : [i32; RAM_SIZE],
    rom : [i32; ROM_SIZE],
}

#[derive(Serialize)]
pub struct ComputerForJs {
    a : i32,
    d : i32,
    m : i32,
    pc : u32,
    ram : Vec<i32>,
    updated_pixels : Vec<Pixel>,
}

#[derive(Serialize)]
pub struct Pixel {
    x : i32,
    y : i32,
    color: &'static str,
}

static mut computer: Computer = Computer {
    a : 0,
    d : 0,
    m : 0,
    pc : 0,
    ram : [0; RAM_SIZE],
    rom : [0; ROM_SIZE],
};

#[wasm_bindgen]
pub fn initialize() {
    unsafe {
    let window = web_sys::window().expect("global window does not exists");    
    let document = window.document().expect("expecting a document on window");
    print("initializing keyboard listeners", "");
    let on_keydown = EventListener::new(&document, "keydown", move |event| {
        let event = event.clone().dyn_into::<web_sys::KeyboardEvent>().unwrap_throw();
        let key_code = event.key_code();
        // print("key down", key_code);
        computer.ram[KEYBOARD] = key_code as i32;
    });
    let on_keyup = EventListener::new(&document, "keyup", |_| {
        // print("key up", "");
        computer.ram[KEYBOARD] = 0;
    });
    on_keydown.forget();
    on_keyup.forget();
    }
}

#[wasm_bindgen]
pub fn edit_rom(values: Vec<i32>) {
    unsafe {
    // reset all rom registers to 0
    computer.rom = [0; ROM_SIZE];
    // set the edited registers
    for (i, v) in values.iter().enumerate() {
        computer.rom[i] = *v;
    }
    }
}

#[wasm_bindgen]
pub fn edit_ram(index: usize, value: i32) {
    unsafe {
    computer.ram[index] = value;
    }
}

#[wasm_bindgen]
pub fn reset() {
    unsafe {
    computer.pc = 0;
    }
}

#[wasm_bindgen]
pub fn step(ram_display_size: usize, cycles: usize) -> JsValue {
    unsafe {
    let mut updated_pixels = Vec::new();
    for _ in 0..cycles {
        match computer.rom.get(computer.pc as usize) {
            Some(instruction) => {
                // print("instruction", instruction);
                let op_code = get_bit(31, *instruction);
                // print("op_code", op_code);
                if !op_code {
                    step_a_instruction(*instruction);
                } else {
                    updated_pixels.extend(step_c_instruction(drop_bits(19, *instruction)));
                }
            }
            None => {}
        }
    }
    let new_computer =
        ComputerForJs {
            a : computer.a,
            d : computer.d,
            m : computer.m,
            pc : computer.pc,
            ram : computer.ram[..ram_display_size].to_vec(),
            updated_pixels : updated_pixels,
        };
    
    JsValue::from_serde(&new_computer).unwrap()
    }
}


unsafe fn step_a_instruction(number: i32) {
    computer.pc = computer.pc + 1;
    computer.a = number;
    match computer.ram.get(number as usize) {
        Some(value) => {
            computer.m = *value;
        },
        None => {}
    }
}

unsafe fn step_c_instruction(instruction: i32) -> Vec<Pixel> {
    let computation_bits = ((instruction as u32) >> 6).try_into().unwrap();
    let destinations_bits = ((instruction as u32 & 0x38) >> 3).try_into().unwrap();
    let jump_bits = instruction & 0x7;

    // print("finished splitting bits", "");

    let computation_result = compute(computation_bits);
    // print("finished compute()", "");
    let updated_pixels = store_computation_result(destinations_bits, computation_result);
    // print("finished store_computation_result()", "");
    move_program_counter(jump_bits, computation_result);
    // print("finished move_program_counter()", "");

    updated_pixels
}

unsafe fn compute(computation_bits: i32) -> i32 {
    let d = computer.d;
    let a = computer.a;
    let m = computer.m;

    match computation_bits {
        0b0101010 => 0,
        0b0111111 => 1,
        0b0111010 => -1,
        0b0001100 => d,
        0b0110000 => a,
        0b0001101 => !d,
        0b0110001 => !a,
        0b0001111 => -d,
        0b0110011 => -a,
        0b0011111 => d + 1,
        0b0110111 => a + 1,
        0b0001110 => d - 1,
        0b0110010 => a - 1,
        0b0000010 => d + a,
        0b0010011 => d - a,
        0b0000111 => a - d,
        0b0000000 => d & a,
        0b0010101 => d | a,
        0b1110000 => m,
        0b1110001 => !m,
        0b1110011 => -m,
        0b1110111 => m + 1,
        0b1110010 => m - 1,
        0b1000010 => d + m,
        0b1010011 => d - m,
        0b1000111 => m - d,
        0b1000000 => d & m,
        0b1010101 => d | m,
        _ => 0, // invalid computation bits
    }
}


unsafe fn store_computation_result(destinations_bits: i32, result: i32) -> Vec<Pixel> {
    let store_to_a_register = get_bit(2, destinations_bits);
    let new_a_register =
        if store_to_a_register {
            result
        } else {
            computer.a
        };
    
    let store_to_d_register = get_bit(1, destinations_bits);
    let new_d_register =
        if store_to_d_register {
            result
        } else {
            computer.d
        };
    
    let store_to_m_register = get_bit(0, destinations_bits);
    let new_m_register =
        if store_to_m_register {
            result
        } else {
            computer.m
        };
    
    // print("computer.a", computer.a);
    // print("computer.m", new_m_register);
    // print("computer.ram.len()", computer.ram.len());
    if (computer.a as usize) < computer.ram.len() {
        computer.ram[computer.a as usize] = new_m_register;
    }

    let mut updated_pixels = Vec::new();

    if store_to_m_register {
        if SCREEN as i32 <= computer.a
        && computer.a < KEYBOARD as i32 {
            let width = 320 / 4;
            // print("width", width);
            let offset = computer.a - SCREEN as i32;
            // print("offset", offset);
            let x = 4 * (offset % width);
            // print("x", x);
            let y = offset / width;
            // print("y", y);
            let all_colors = new_m_register as u32;
            // print("all_colors", all_colors);

            let x1 = x;
            let color1 = css_color_from_8_bits(((all_colors & 0xFF000000) >> 24).try_into().unwrap());
            updated_pixels.push(Pixel { x : x1, y : y, color : color1 });
            // print("AL: color1", color1);

            let x2 = x + 1;
            let color2 = css_color_from_8_bits(((all_colors & 0x00FF0000) >> 16).try_into().unwrap());
            updated_pixels.push(Pixel { x : x2, y : y, color : color2 });
            // print("AL: color2", color2);
            
            let x3 = x + 2;
            let color3 = css_color_from_8_bits(((all_colors & 0x0000FF00) >> 8).try_into().unwrap());
            updated_pixels.push(Pixel { x : x3, y : y, color : color3 });
            // print("AL: color3", color3);
            
            let x4 = x + 3;
            let color4 = css_color_from_8_bits((all_colors & 0x000000FF).try_into().unwrap());
            updated_pixels.push(Pixel { x : x4, y : y, color : color4 });
            // print("AL: color4", color4);
        }
    }

    computer.a = new_a_register;
    computer.d = new_d_register;
    computer.m = new_m_register;

    // print("successfully stored to ram", ());

    updated_pixels
}


unsafe fn move_program_counter(jump_bits: i32, computation_result: i32) {
    let zr = computation_result == 0;
    let ng = computation_result < 0;

    let lt = get_bit(2, jump_bits);
    let eq = get_bit(1, jump_bits);
    let gt = get_bit(0, jump_bits);

    let jump =
        (if lt { ng } else { false })
        || (if eq { zr } else { false })
        || (if gt { !ng && !zr } else { false });
    
    if jump {
        computer.pc = computer.a as u32;
    } else {
        computer.pc = computer.pc + 1;
    }
}

// source code from: https://github.com/bilalq/eight-bit-color-picker/blob/master/lib/eight-bit-color-picker.js
static color_palette: [&'static str; 256] = [
    "#400000", "#400000", "#400900", "#234000", "#004000", "#004000", "#004000",
    "#000d40", "#000040", "#000040", "#000040", "#000040", "#280040", "#400003",
    "#400000", "#000000", "#540000", "#540000", "#541d00", "#375400", "#005400",
    "#005400", "#005402", "#002154", "#000054", "#000054", "#000054", "#000054",
    "#3c0054", "#540017", "#540000", "#0d0d0d", "#680000", "#680000", "#683100",
    "#4b6800", "#006800", "#006800", "#006816", "#003568", "#001168", "#000068",
    "#000068", "#000068", "#500068", "#68002b", "#680000", "#212121", "#7c0000",
    "#7c0000", "#7c4500", "#5f7c00", "#0b7c00", "#007c00", "#007c2a", "#00497c",
    "#00257c", "#00007c", "#00007c", "#10007c", "#64007c", "#7c003f", "#7c0000",
    "#353535", "#900000", "#900400", "#905900", "#739000", "#1f9000", "#009000",
    "#00903e", "#005d90", "#003990", "#000090", "#000090", "#240090", "#780090",
    "#900053", "#900000", "#494949", "#a40000", "#a41800", "#a46d00", "#87a400",
    "#33a400", "#00a400", "#00a452", "#0071a4", "#004da4", "#0000a4", "#0000a4",
    "#3800a4", "#8c00a4", "#a40067", "#a40013", "#5d5d5d", "#b80000", "#b82c00",
    "#b88100", "#9bb800", "#47b800", "#00b800", "#00b866", "#0085b8", "#0061b8",
    "#000db8", "#0000b8", "#4c00b8", "#a000b8", "#b8007b", "#b80027", "#717171",
    "#cc0000", "#cc4000", "#cc9500", "#afcc00", "#5bcc00", "#06cc00", "#00cc7a",
    "#0099cc", "#0075cc", "#0021cc", "#0c00cc", "#6000cc", "#b400cc", "#cc008f",
    "#cc003b", "#858585", "#e00000", "#e05400", "#e0a900", "#c3e000", "#6fe000",
    "#1ae000", "#00e08e", "#00ade0", "#0089e0", "#0035e0", "#2000e0", "#7400e0",
    "#c800e0", "#e000a3", "#e0004f", "#999999", "#f41414", "#f46814", "#f4bd14",
    "#d7f414", "#83f414", "#2ef414", "#14f4a2", "#14c1f4", "#149df4", "#1449f4",
    "#3414f4", "#8814f4", "#dc14f4", "#f414b7", "#f41463", "#adadad", "#ff2828",
    "#ff7c28", "#ffd128", "#ebff28", "#97ff28", "#42ff28", "#28ffb6", "#28d5ff",
    "#28b1ff", "#285dff", "#4828ff", "#9c28ff", "#f028ff", "#ff28cb", "#ff2877",
    "#c1c1c1", "#ff3c3c", "#ff903c", "#ffe53c", "#ffff3c", "#abff3c", "#56ff3c",
    "#3cffca", "#3ce9ff", "#3cc5ff", "#3c71ff", "#5c3cff", "#b03cff", "#ff3cff",
    "#ff3cdf", "#ff3c8b", "#d5d5d5", "#ff5050", "#ffa450", "#fff950", "#ffff50",
    "#bfff50", "#6aff50", "#50ffde", "#50fdff", "#50d9ff", "#5085ff", "#7050ff",
    "#c450ff", "#ff50ff", "#ff50f3", "#ff509f", "#e9e9e9", "#ff6464", "#ffb864",
    "#ffff64", "#ffff64", "#d3ff64", "#7eff64", "#64fff2", "#64ffff", "#64edff",
    "#6499ff", "#8464ff", "#d864ff", "#ff64ff", "#ff64ff", "#ff64b3", "#fdfdfd",
    "#ff7878", "#ffcc78", "#ffff78", "#ffff78", "#e7ff78", "#92ff78", "#78ffff",
    "#78ffff", "#78ffff", "#78adff", "#9878ff", "#ec78ff", "#ff78ff", "#ff78ff",
    "#ff78c7", "#ffffff", "#ff8c8c", "#ffe08c", "#ffff8c", "#ffff8c", "#fbff8c",
    "#a6ff8c", "#8cffff", "#8cffff", "#8cffff", "#8cc1ff", "#ac8cff", "#ff8cff",
    "#ff8cff", "#ff8cff", "#ff8cdb", "#ffffff"
    ];

fn css_color_from_8_bits(num: u8) -> &'static str {
    color_palette[num as usize]
}


fn get_bit(index: usize, bits: i32) -> bool {
    ((bits >> index) & 1) == 1
}

fn drop_bits(drop_amount: usize, bits: i32) -> i32 {
    (((bits as u32) << drop_amount) >> drop_amount).try_into().unwrap()
}

fn print<T>(name: &str, thing: T)
where T : Debug
{
    web_sys::console::log_1(&format!("{}: {:#?}", name, thing).into());
}