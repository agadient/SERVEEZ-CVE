#!/usr/bin/python
import socket
from time import sleep
import sys
a = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
b = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
c = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
d = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
f = open(sys.argv[1], 'r')
data = f.read()
f.close()

a.connect(('localhost', 42422))
sleep(0.5)
b.close()
a.send(data)
print(a.recv(1024))
c.recv(1024)
d.recv(1024)

