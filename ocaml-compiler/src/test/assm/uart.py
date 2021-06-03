import sys
sys.path.append('/home/taroy/.local/lib/python3.9/site-packages')

import serial
import tqdm

def serialize(data, text, rodata):
    import copy
    lb = 0xffffffff
    ub = 0
    mem = []
    mem = mem + copy.deepcopy(data)
    mem = mem + copy.deepcopy(text)
    mem = mem + copy.deepcopy(rodata)
    for addr, _ in mem:
        lb = min(lb, addr)
        ub = max(ub, addr)
    offset = lb
    l = (ub-lb+4)//4
    data_arr = [0] * l
    for addr, d32 in mem:
        data_arr[(addr-offset)//4] = d32
    return offset, data_arr

def list_to_bytes(lis):
    blis = list(map(lambda d: (d).to_bytes(4, byteorder='big'), lis))
    # print(blis)
    res = (b'').join(blis)
    # print(type(res), (res[5]).to_bytes(1, byteorder='big'))
    return res
    
def receive(uart):
    pre = b''
    while True:
        res = uart.read(4)
        if res != pre:
            print(list(map(hex, res)))
            pre = res

def communicate(uart, offset, data_arr):
    bar = tqdm.tqdm(total=len(data_arr))
    # print(list(map(hex, data_arr)))
    for i, i32 in enumerate(data_arr):
        addr = 4*i + offset
        arr = [ addr, i32 ]
        # print(arr)
        b = list_to_bytes(arr)
        # print(b)
        import time
        for e in b:
            # print(uart.write((e).to_bytes(1, byteorder='big')))
            uart.write((e).to_bytes(1, byteorder='big'))
            # time.sleep(0.1)
        res = uart.read(8)
        # print(list(map(hex, res)))
        bar.update(1)
    bar.close()
    receive(uart)
    
base = 256

def test(uart):
    data = [0]*5
    for i in range(len(data)):
        data[i] = 0
        if True:
            # data[i] = 0x00b5b500
            # data[i] = 0x00b5b504
            data[i] = 0x000000b5
        else:
            for j in range(4):
                data[i] = data[i] * base + (0xb5)
    communicate(uart, 0xffff + 0x11, data)

if __name__ == '__main__':
    try:
        port = "/dev/ttyUSB0"
        baudrate = 115200
        uart = serial.Serial(port=port, baudrate=baudrate)
        uart.reset_input_buffer()
        a = 0
        if a == 1:
            test(uart)
        else:
            from parser import Parser
            import fileinput
            p = Parser()
            for line in fileinput.input():
                p.read_line(line)
            offset, data = serialize(p.data, p.text, p.rodata)
            communicate(uart, offset, data)
    except:
        print("except")
    finally:
        uart.close()
