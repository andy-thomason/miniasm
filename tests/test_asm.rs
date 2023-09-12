use miniasm::x86::{Instruction, EncodeResult};

#[test]
fn test_asm() {
    println!("{:?}", std::env::current_dir());
    // let text = include_str!("../asm/x86/testall.list");
    let text = std::fs::read_to_string("tests/testall.list").unwrap();

    let mut prev_bytes = EncodeResult::new();
    for line in text.split('\n') {
        println!("{line}");
        let mut line = line.split_ascii_whitespace();
        let Some(v) = line.next() else { continue };
        let Ok(_line_number) = v.parse::<u32>() else { continue };
        let Some(src) = line.next() else { continue };
        let Ok(_address) = u64::from_str_radix(src, 16) else { continue };

        let mut bytes : Vec<u8> = vec![];
        let mut asm = String::new();
        while let Some(src) = line.next() {
            if src.bytes().any(|b| b.is_ascii_lowercase()) {
                asm.push_str(src);
                break;
            } else if let Ok(w) = u32::from_str_radix(src, 16) {
                let len = src.len();
                bytes.extend(&w.to_be_bytes()[4-len/2..4]);
            } else {
                asm.push_str(src);
                break;
            }
        }

        while let Some(src) = line.next() {
            asm.push_str(" ");
            asm.push_str(src);
        }
    
        println!("gnu:     {:02x?} asm: {}", hex::encode(&bytes), asm);

        let ins = Instruction::from_str(&asm).expect("fail asm");
        let obytes = ins.encode().expect("encode error");

        println!("miniasm: {:02x?} asm: {}", hex::encode(obytes.as_slice()), ins);

        assert_eq!(obytes.as_slice(), &bytes);

        // Generated instructions must be monotonic.
        assert!(obytes.as_slice() > prev_bytes.as_slice());
        prev_bytes = obytes;
    }
}
