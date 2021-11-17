use octoproc_assembler::parser;

fn main() {
    println!("{:?}", parser::parse("uwu.s", "
lli x0, 123
addi x0, x1, 12
add x0, x1, x2
label:
label2: load x0, 2(x1)
jal x0, label
blt x0, x1, label2
jalr x1, x2
"));
}
