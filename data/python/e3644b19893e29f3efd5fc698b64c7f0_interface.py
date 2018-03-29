"""interface module"""

import string
import sys
import cPickle
import commands
import model
import world
import traceback
import cli
import actor
import messages
import stackless
import game.exceptions 

class Color():

    def __init__(self):
        self.color()
        self.cli = cli.CommandLineInterface(tabCallBack=self.getCommands)

    def color(self):
        """Enable color"""
        self.clear = "\x1B[0m"
        self.bold = "\x1B[1m"
        self.italics = "\x1B[3m"
        self.underline = "\x1B[4m"
        self.inverse = "\x1B[7m"
        self.black = "\x1B[30m"
        self.red = "\x1B[31m"
        self.green = "\x1B[32m"
        self.yellow = "\x1B[33m"
        self.blue = "\x1B[34m"
        self.magenta = "\x1B[35m"
        self.cyan = "\x1B[36m"
        self.white = "\x1B[37m"
        self.black_background = "\x1B[40m"
        self.red_background = "\x1B[41m"
        self.green_background = "\x1B[42m"
        self.yellow_background = "\x1B[43m"
        self.blue_background = "\x1B[44m"
        self.magenta_background = "\x1B[45m"
        self.cyan_background = "\x1B[46m"
        self.white_background = "\x1B[47m"

    def nocolor(self):
        """Disable color"""
        self.clear = ""
        self.bold = ""
        self.italics = ""
        self.underline = ""
        self.inverse = ""
        self.black = ""
        self.red = ""
        self.green = ""
        self.yellow = ""
        self.blue = ""
        self.magenta = ""
        self.cyan = ""
        self.white = ""
        self.black_background = ""
        self.red_background = ""
        self.green_background = ""
        self.yellow_background = ""
        self.blue_background = ""
        self.magenta_background = ""
        self.cyan_background = ""
        self.white_background = ""

class Interface(Color,actor.Actor):

    def __init__(self,world):
        Color.__init__(self)
        actor.Actor.__init__(self)
        self.memory = { }
        self.world = world
        self.object = None
        self.out = sys.stdout
        self.room = None
        self.open = True

    def lookup(self,x, notFound=None):
        if self.memory.has_key(x):
            return self.memory[x]
        else:
            return notFound

    def getCommand(self,command):
        if self.commands.has_key(command):
            return self.commands[command]
        else:
            return None

    def applyCommand(self,command,args):
        apply(command,[ self ] + args)

    def write(self,text):
        self.out.write(text)
        self.out.flush()
    
    def writePrompt(self):
        self.write( "\n%s%s%s>%s" % (self.red, self.underline, self.object, self.clear ))

    def getCommandLine(self):
        return self.cli.getCommand()

    def prompt(self):
        self.writePrompt()
        commandline = self.getCommandLine()
        try:
            splitcommand = string.split(commandline)
            if len(splitcommand) == 0: return
            command = self.getCommand(splitcommand[0])
            if command:
                self.applyCommand(command,splitcommand[1:])
                return
            if self.object:
                command = self.object.getCommand(splitcommand[0])
                if command:
                    self.object.updateCommand = [ command ] + splitcommand[1:]
                    return
            self.write( "\n%sWhat?%s" % (self.red,self.clear) )
        except Exception, error:
            self.write( "\n%sWhat?%s" % (self.red,self.clear) )
            self.write( "\nError: %s" % error ) 
            traceback.print_tb(sys.exc_info()[2])
        finally:
            pass
    
    def getCommands(self,prefix):
        commands = []
        commands += self.commands.items()
        if self.object:
            commands += self.object.getCommands()
        return filter(lambda x: x[0].startswith(prefix),commands)

    def help(self):
        """Print the help for commands"""
        self.write( "\nCommands: " )
        for (name,command) in sorted(self.commands.items()):
            self.write( "\n%-*s - %s" % (10,name,command.__doc__) )
        if self.object:
            for (name,command) in sorted(self.object.getCommands()):
                self.write( "\n%-*s - %s" % (10,name,command.__doc__) )

    def objects(self):
        """Get a list of all the objects"""
        self.write("World Objects\n")
        for x in self.world.world.keys():
            self.write( "\n%s: %s" % (x,self.world.getObject(x)) )

    def setobject(self,x):
        """Change the object you control"""
        if self.object: self.object.interface = None
        self.object = self.world.getObject(x)
        if self.object: self.object.interface = self

    def create(self,type="Character"):
        """Create a new object"""
        o = eval("model.%s()" % (type,))
        if self.room: self.room.addObject(o.id)
        if self.room: o.location = self.room.id
        self.write( "\nCreated %s %s" % (o, o.id))

    def close(self):
        self.open = False

    def exit(self):
        """Exit the game"""
        self.world.close()
        self.close()
        raise game.exceptions.GameExit()

    def reload(self):
        """Reload the code"""
        reload(cli)
        reload(model)
        self.write( "\nReload complete" )

    def sync(self):
        """Sync the world to disk"""
        self.world.sync()

    def run(self):
        """Get a command"""
        while self.open:
            try:
                self.prompt()
            except Exception, error:
                self.write( "\nError: %s" % error ) 
                traceback.print_tb(sys.exc_info()[2])
            #print "interface schedule"
            stackless.schedule()

    def display(self,text):
        self.cli.disableRaw()
        self.write("\n")
        self.write(text)
        self.write("\n")
        self.writePrompt()
        self.cli.enableRaw()

Interface.commands = {
             '?'         : Interface.help,
             'help'      : Interface.help,
             'color'     : Interface.color,
             'nocolor'   : Interface.nocolor, }


class LocalInterface(Interface):

    def __init__(self,world):
        Interface.__init__(self,world)

LocalInterface.commands = {
             'exit'      : Interface.exit,
             'sync'      : Interface.sync,
             '?'         : Interface.help,
             'help'      : Interface.help,
             'color'     : Interface.color,
             'nocolor'   : Interface.nocolor,
             'objects'   : Interface.objects,
             'setobject' : Interface.setobject, 
             'create'    : Interface.create, }


