import threading

class minhaThread (threading.Thread):
    def __init__(self, threadID, nome, contador):
        threading.Thread.__init__(self)
        self.threadID = threadID
        self.nome = nome
        self.contador = contador
    def run(self):
        print "Iniciando thread %s com %d processos" % (self.name,self.contador)
        processo(self.nome, self.contador)
        print "Finalizando " + self.nome

def processo(nome, contador):
    while contador:
        print "Thread %s fazendo o processo %d" % (nome, contador)
        contador -= 1

# Criando as threads
thread1 = minhaThread(1, "Alice", 8)
thread2 = minhaThread(2, "Bob", 8)

# Start new Threads
thread1.start()
thread2.start()

threads = []
threads.append(thread1)
threads.append(thread2)

for t in threads:
    t.join()

print "Saindo da main"
