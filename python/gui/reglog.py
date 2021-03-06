import Tkinter
import matplotlib
matplotlib.use("TkAgg")
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg, NavigationToolbar2TkAgg
from matplotlib.figure import Figure

def reglog(n0,r,alpha,tempo):
    x=[0]
    y=[n0]
    for i in range(0,tempo):
        x.append(i)
        y.append(y[i]+y[i]*r*(1-alpha*y[i]))
    return x,y

tela = Tkinter.Tk()

n0=1
r=1
alpha=0.01
tempo=20
x,y=reglog(n0,r,alpha,tempo)

f = Figure(figsize=(5,5), dpi=100)
a = f.add_subplot(111)
a.plot(x,y)
canvasf = FigureCanvasTkAgg(f,master=tela)
canvasf.show()

#Criando Entradas
E1 = Tkinter.Entry(tela, bd =5)
def get(event):
    novo=float(event.widget.get())
    x,y=reglog(n0,novo,alpha,tempo)
    a.clear()
    a.plot(x,y)
    canvasf.show()
    canvasf.get_tk_widget().pack()

E1.bind('<Return>', get)


w = Tkinter.Label(tela, text="Taxa de crescimento")
w.pack(side="left")
E1.pack(side="left")
canvasf.get_tk_widget().pack()

tela.mainloop()

print "Fim"





print "Fim"
