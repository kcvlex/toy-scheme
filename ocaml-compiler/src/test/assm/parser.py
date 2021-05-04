import re

def check_prefix(prefix, s):
    l = len(prefix)
    if len(s) < l:
        return False
    return s[:l] == prefix

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
        self.section = None

    def read_content(self, line):
        if self.section is None:
            return
        elif self.section == symtbl:
            # bss
            mark = ' .bss'
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


if __name__ == '__main__':
    parser = Parser()
    import fileinput
    for line in fileinput.input():
        parser.read_line(line)
    parser.dump()
