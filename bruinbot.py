import sys 
import socket 
import string 
import os #not necessary but later on I am going to use a few features from this 

HOST='irc.freenode.net' #The server we want to connect to 
PORT=6667 #The connection port which is usually 6667 
NICK='bruinbot' #The bot's nickname 
IDENT='bruinbot' 
REALNAME='PENIS' 
OWNER='DICKBAG' #The bot owner's nick 
CHANNELINIT='#reddit-ucla' #The default channel for the bot 
readbuffer='' #Here we store all the messages from server 

s=socket.socket( ) #Create the socket 
s.connect((HOST, PORT)) #Connect to server 
s.send('NICK '+NICK+'n') #Send the nick to server 
s.send('USER '+IDENT+' '+HOST+' bla :'+REALNAME+'n') #Identify to server 

####################################################
#Listens to messages in an infinite loop
####################################################

while 1: 

        line=s.recv(500) #recieve server messages 
    print line #server message is output 
    if line.find('Welcome to the UnderNet IRC Network')!=-1: #This is Crap(I wasn't sure about it but it works) 
        s.send('JOIN '+CHANNELINIT+'n') #Join a channel 
    if line.find('PRIVMSG')!=-1: #Call a parsing function 
        parsemsg(line) 
        line=line.rstrip() #remove trailing 'rn' 
        line=line.split() 
        if(line[0]=='PING'): #If server pings then pong 
            s.send('PONG '+line[1]+'n') 

####################################################
#Parsing function. Message is stored in msgpart.
####################################################

def parsemsg(msg): 
    complete=msg[1:].split(':',1) #Parse the message into useful data 
    info=complete[0].split(' ') 
    msgpart=complete[1] 
    sender=info[0].split('!') 
    if msgpart[0]=='`' and sender[0]==OWNER: #Treat all messages starting with '`' as command 
        cmd=msgpart[1:].split(' ') 
        if cmd[0]=='op': 
            s.send('MODE '+info[2]+' +o '+cmd[1]+'n') 
        if cmd[0]=='deop': 
            s.send('MODE '+info[2]+' -o '+cmd[1]+'n') 
        if cmd[0]=='voice': 
            s.send('MODE '+info[2]+' +v '+cmd[1]+'n') 
        if cmd[0]=='devoice': 
            s.send('MODE '+info[2]+' -v '+cmd[1]+'n') 
        if cmd[0]=='sys': 
            syscmd(msgpart[1:],info[2]) 
         
    if msgpart[0]=='-' and sender[0]==OWNER : #Treat msgs with - as explicit command to send to server 
        cmd=msgpart[1:] 
        s.send(cmd+'n') 
        print 'cmd='+cmd 

####################################################
#Executes system commands.
####################################################

def syscmd(commandline,channel): 
    cmd=commandline.replace('sys ','') 
    cmd=cmd.rstrip() 
    os.system(cmd+' >temp.txt') 
    a=open('temp.txt') 
    ot=a.read() 
    ot.replace('n','|') 
    a.close() 
    s.send('PRIVMSG '+channel+' :'+ot+'n') 
    return 0 
