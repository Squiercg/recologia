'''
import serial
ser = serial.Serial('/dev/ttyACM0', 9600)
while True:
    print ser.readline()
'''

import Tkinter
import matplotlib
matplotlib.use("TkAgg")
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.figure import Figure

tela = Tkinter.Tk()

f = Figure(figsize=(5,5), dpi=100)
a = f.add_subplot(111)
x=[1,2,3,4,5,6,7,8]
y=[5,6,1,3,8,9,3,5]
a.plot(x,y)
canvasf = FigureCanvasTkAgg(f,master=tela)
canvasf.show()
canvasf.get_tk_widget().pack()

#Criando Entradas
E1 = Tkinter.Entry(tela, bd =5)
def get(event):
    novo=float(event.widget.get())
    print novo
    x.append(x[-1]+1)
    y.append(novo)
    a.plot(x,y)
    canvasf.show()
    canvasf.get_tk_widget().pack()

E1.bind('<Return>', get)
E1.pack()

tela.mainloop()

print "Fim"
