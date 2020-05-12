mod utils;

use wasm_bindgen::prelude::*;
use std::convert::TryInto;
extern crate serde_json;
extern crate web_sys;
use std::fmt::Debug;

#[macro_use]
extern crate serde_derive;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[derive(Serialize, Deserialize, Debug)]
pub struct Computer {
    a : i32,
    d : i32,
    m : i32,
    pc : u32,
    ram : Vec<i32>,
    rom : Vec<i32>,
    #[serde(skip_deserializing)]
    updated_pixels : Vec<Pixel>
  }

#[derive(Serialize, Debug)]
pub struct Pixel {
    x : i32,
    y : i32,
    color: (u8, u8, u8),
}

#[wasm_bindgen]
pub fn step(cycles: usize, computer_in_json: &JsValue) -> JsValue {
    let mut computer = decode_computer(computer_in_json);
    for _ in 0..cycles {
        // print("pc: ", computer.pc);
        match computer.rom.get(computer.pc as usize) {
            Some(instruction) => {
                // print("instruction", instruction);
                let op_code = get_bit(31, *instruction);
                // print("op_code", op_code);
                if !op_code {
                    step_a_instruction(*instruction, &mut computer);
                } else {
                    step_c_instruction(drop_bits(19, *instruction), &mut computer);
                }
            }
            None => {}
        }
    }
    JsValue::from_serde(&computer).unwrap()
}


fn decode_computer(computer_in_json: &JsValue) -> Computer {
    match computer_in_json.into_serde() {
      Ok(computer) => computer,
      Err(err) => {
        web_sys::console::log_1(&format!("{:#?}", err).into()); 
        Computer {
            a : 0,
            d : 0,
            m : 0,
            pc : 0,
            ram : Vec::new(),
            rom : Vec::new(),
            updated_pixels : Vec::new()
        }
      },
    }
}


fn step_a_instruction(number: i32, computer: &mut Computer) {
    computer.pc = computer.pc + 1;
    computer.a = number;
    match computer.ram.get(number as usize) {
        Some(value) => {
            computer.m = *value;
        },
        None => {}
    }
}

fn step_c_instruction(instruction: i32, computer: &mut Computer) {
    let computation_bits = ((instruction as u32) >> 6).try_into().unwrap();
    let destinations_bits = ((instruction as u32 & 0x38) >> 3).try_into().unwrap();
    let jump_bits = instruction & 0x7;

    let computation_result = compute(computation_bits, computer);

    move_program_counter(jump_bits, computation_result, computer);
    store_computation_result(destinations_bits, computation_result, computer);
}

fn compute(computation_bits: i32, computer: &Computer) -> i32 {
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


fn store_computation_result(destinations_bits: i32, result: i32, computer: &mut Computer) {
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
    // print("successfully stored to ram", ());

    if store_to_m_register {
        if 2i32.pow(16) <= computer.a
        && computer.a <= 2i32.pow(16) + 2i32.pow(15) {
            let width = 512 / 4;
            // print("width", width);
            let offset = computer.a - 2i32.pow(16);
            // print("offset", offset);
            let x = 4 * (offset % width);
            // print("x", x);
            let y = offset / width;
            // print("y", y);
            let all_colors = new_m_register as u32;
            // print("all_colors", all_colors);

            let x1 = x;
            let color1 = css_color_from_8_bits((all_colors & 0xFF000000 >> 24).try_into().unwrap());
            computer.updated_pixels.push(Pixel { x : x1, y : y, color : color1 });

            let x2 = x + 1;
            let color2 = css_color_from_8_bits((all_colors & 0xFF000000 >> 16).try_into().unwrap());
            computer.updated_pixels.push(Pixel { x : x2, y : y, color : color2 });
            
            let x3 = x + 2;
            let color3 = css_color_from_8_bits((all_colors & 0xFF000000 >> 8).try_into().unwrap());
            computer.updated_pixels.push(Pixel { x : x3, y : y, color : color3 });
            
            let x4 = x + 3;
            let color4 = css_color_from_8_bits((all_colors & 0xFF000000).try_into().unwrap());
            computer.updated_pixels.push(Pixel { x : x4, y : y, color : color4 });
        }
    }

    computer.a = new_a_register;
    computer.d = new_d_register;
    computer.m = new_m_register;
}


fn move_program_counter(jump_bits: i32, computation_result: i32, computer: &mut Computer) {
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


fn css_color_from_8_bits(num: u8) -> (u8, u8, u8) {
    let r = (num >> 5) * 255 / 7;
    let g = ((num >> 2) & 0x07) * 255 / 7;
    let b = (num & 0x03) * 255 / 3;
    return (r, g, b)
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