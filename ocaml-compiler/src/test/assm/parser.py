import re

DEBUG = True

def check_prefix(prefix, s):
    l = len(prefix)
    if len(s) < l:
        return False
    return s[:l] == prefix

def check_suffix(suffix, s):
    l = len(suffix)
    if len(s) < l:
        return False
    return s[-l:] == suffix

text = 'text'
data = 'data'
rodata = 'rodata'
bss = 'bss'
symtbl = 'SYMBOL TABLE'
label_prefix = 'Contents of section .'

def check_section(sec, s):
    label = label_prefix + sec
    return check_prefix(label, s)

def eat(s, c):
    assert s[0] == c
    return s[1:]

def eat_head_num(s):
    i = 0
    while s[i] != ' ':
        i += 1
    return s[:i], s[i+1:]

base = [ 1, (1 << 8), (1 << 16), (1 << 24) ]

def little_endian(s):
    ret = 0
    for i in range(4):
        j = 2 * i
        n = s[j:j+2]
        n = int(n, 16)
        ret += n * base[i]
    return ret

class Parser:
    def __init__(self):
        self.text = []
        self.data = []
        self.rodata = []
        self.bss = []
        self.entry = -1
        self.display = -1
        self.section = None

    def read_content(self, line):
        if self.section is None:
            return
        elif self.section == symtbl:
            # bss
            mark = ' .bss'
            if check_suffix('__display', line):
                addr = int(line.split(' ')[0], 16)
                self.display = addr
                return
            if not (mark in line):
                return
            line = re.sub(r'\s*$', '', line)
            lis = re.sub(r'\s+', ' ', line).split(' ')
            addr = int(lis[0], 16)
            size = int(lis[-2], 16)

            # ignore debug info
            if 0 < size:
                self.bss.append((addr, size))
        else:
            line = eat(line, ' ')
            addr, line = eat_head_num(line)
            addr = int(addr, 16)
            i = 0
            while i < 4:
                if line[0] == ' ':
                    break
                instr, line = eat_head_num(line)
                instr = little_endian(instr)
                ele = (addr + 4 * i, instr)
                if self.section == text:
                    self.text.append(ele)
                elif self.section == data:
                    self.data.append(ele)
                elif self.section == rodata:
                    self.rodata.append(ele)
                else:
                    assert False
                i += 1

    def read_line(self, line):
        line = re.sub(r'\n', '', line)
        if check_prefix('start address', line):
            addr = line.split(' ')[-1]
            addr = int(addr, 16)
            self.entry = addr
        elif check_prefix(symtbl, line):
            self.section = symtbl
        elif check_section(text, line):
            self.section = text
        elif check_section(data, line):
            self.section = data
        elif check_section(rodata, line):
            self.section = rodata
        elif check_prefix(label_prefix, line):
            self.section = None
        else:
            self.read_content(line)

    def dump_aux(self, label, lis):
        print(label)
        for addr, instr in lis:
            print(hex(addr), hex(instr))

    def dump(self):
        print('entry : ', hex(self.entry))
        self.dump_aux(text, self.text)
        self.dump_aux(data, self.data)
        self.dump_aux(rodata, self.rodata)
        print('bss')
        for addr, size in self.bss:
            print(hex(addr), hex(size))

head = """
`default_nettype none 

module top();
    reg CLK, RST_X;
    initial begin CLK = 1; forever #50 CLK = ~CLK; end
    initial begin 
        RST_X = 0; 
        #2000 RST_X = 1;
        #100  RST_X = 0;
    end
    initial begin #10000000 $finish(); end
    initial begin $dumpfile("wave.vcd"); $dumpvars(0, p); end
"""

def gen_tail():
    if DEBUG:
        return """
    always @(p.PC) begin if (RST_X && p.PC == 0) $finish(); end

    wire dram_wr, dram_rd;
    wire [24:0] dram_addr;
    wire [15:0] dram_rd_data, dram_wr_data;
    
    always @(posedge CLK) begin
        #30 $write("%d : PC=%x rs1=%d rs2=%d rd=%d addr=%x read=%x\\n",
                $stime-30, 
                p.PC,
                p.cpu.rs1, p.cpu.rs2, p.cpu.rd, 
                dram_addr, dram_rd_data);
    end

    PROCESSOR p(
        .clk(CLK), 
        .reset(RST_X)
    );
endmodule
    """
    else:
        return """
    always @(p.PC) begin if (RST_X && p.PC == 0) $finish(); end

    wire dram_wr, dram_rd;
    wire [24:0] dram_addr;
    wire [15:0] dram_rd_data, dram_wr_data;

    PROCESSOR p(
        .clk(CLK), 
        .reset(RST_X)
    );
endmodule
    """

tail = gen_tail()

def fix_hex(h, xlen):
    h = hex(h)[2:]
    prefix = "{}'h".format(xlen)
    return prefix + h

base8 = 1 << 8;
base16 = 1 << 16

def set_value(addr, data):
    b_and = base8 - 1
    format_s = "        p.sdram.mem[{}] = {};"
    lis = []
    for i in range(4):
        s = format_s.format(fix_hex(addr + (3 - i), 25), fix_hex(data & b_and, 8))
        lis.append(s)
        data >>= 8
    return '\n'.join(reversed(lis))

class Generator:
    def __init__(self, parser):
        self.parser = parser

    def set_imem(self):
        return [ set_value(addr, instr) for addr, instr in self.parser.text ]

    def set_data(self):
        import copy
        lis = copy.deepcopy(self.parser.data)
        lis.extend(self.parser.rodata)
        return [ set_value(addr, data) for addr, data in lis ]

    def set_display(self):
        return '    always @(p.PC) if (p.PC == {}) $write("display : %d\\n", p.cpu.regfile.x[10]);'.format(self.parser.display)

    def gen(self):
        imem = '\n'.join(self.set_imem())
        data = '\n'.join(self.set_data())
        display = self.set_display()
        body = '\n'.join(
                [ "    initial begin",
                  "        p.PC = {};".format(fix_hex(parser.entry, 32)),
                  imem,
                  data,
                  "    end" ])
        return '\n'.join([head, body, display, tail])


if __name__ == '__main__':
    parser = Parser()
    import fileinput
    for line in fileinput.input():
        parser.read_line(line)
    generator = Generator(parser)
    print(generator.gen())
