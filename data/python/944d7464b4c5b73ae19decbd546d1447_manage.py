
# manage.py

from flaskext.script import Manager

from myapp import app
from myojin.custom_script import Manager
manager = Manager(app)
    
if __name__ == "__main__":
    manager.run()
