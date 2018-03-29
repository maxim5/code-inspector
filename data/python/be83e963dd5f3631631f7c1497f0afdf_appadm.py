"""
	Manage different applications.
	We don't use python.mysql here, because we want this to work before that.
	it's not for running queries!

	Created by Craig Sawyer on 2010-01-14.
	Copyright (c) 2009, 2010 Craig Sawyer (csawyer@yumaed.org). All rights reserved. see LICENSE.
"""
import logging
import os,pwd

from cifitlib.files import run, getFilename, append
from cifitlib.classes import classes

log = logging.getLogger('%s:app' % classes['hostname'])

class SysADM(object):
	"""Generic system administration stuff"""

	def findSSHKeyFile(self,user):
		"""return ssh key file if it exists, or False"""
		homedir = pwd.getpwnam(user)[5]
		sshdir = os.path.join(homedir,'.ssh')
		keyfile = os.path.join(sshdir,'id_dsa.pub')
		if os.path.exists(keyfile):
			return keyfile
		else:
			keyfile = os.path.join(sshdir,'id_rsa.pub')
			if os.path.exists(keyfile):
				return keyfile
		return False

	def genSSHKey(self,user):
		"""generate a user SSH key, assuming it's not already created.
		returns full path to public key file.
		"""
		homedir = pwd.getpwnam(user)[5]
		sshdir = os.path.join(homedir,'.ssh')
		keyfile = self.findSSHKeyFile(user)
		if keyfile:
			return keyfile
		#otherwise, we need to generate it.
		if not os.path.exists(sshdir):
			os.makedirs(sshdir)
		run('sudo -u %s ssh-keygen -q -t rsa -N "" -f %sid_rsa' %
	  (user,sshdir+os.sep) )
		keyfile = self.findSSHKeyFile(user)
		if keyfile:
			return keyfile
		else:
			raise 'error creating sshKeyFile for user: %s'
			return False

	def addSSHKey(self,keyfile,user):
		"""Add keyfile ssh key to authorized_keys2 to the specified username"""
		#by default we don't succeed.
		ret = False
		keyfile = getFilename(keyfile)
		homedir = pwd.getpwnam(user)[5]
		installdir = os.path.join(homedir,'.ssh')
		installfile = os.path.join(installdir,'authorized_keys2')
		if not os.path.exists(installdir):
			log.info('making .ssh dir for: %s' % user)
			os.makedirs(installdir)
		if os.path.exists(keyfile):
				key = open(keyfile,'r').read()
				ret = append(installfile,[key])
		else:
			log.critical("keyfile does not exist: %s" % keyfile)
		return ret

class MysqlADM(object):
	"""Manage MySQL.

	We don't use the mysql module, we are after managing the mysql server system,
	not necessarily databases within MySQL. Use something like Liquibase for that.
	"""
	def __init__(self, rootpw=None):
		"""initialize object"""
		self.rootpw = None

	def runAdmin(self, arg):
		"""run mysqladmin util with cmd as ''"""
		cmd = "mysqladmin %s " % (arg)
		if self.rootpw:
			cmd += "--password=%s" % (self.rootpw)
		ret, out = run(cmd)
		if ret:
			log.error('problem with mysqladmin:%s' % arg)
		else:
			return out

	def dumpAll(self, filename):
		"""dump all databases to given filename"""
		cmd = "mysqldump"
		if self.rootpw:
			cmd += " --password=%s" % (self.rootpw)
		cmd += " --all-databases > %s" % getFilename(filename)
		ret, out = run(cmd)
		if ret:
			log.error('problem with dumpAll:%s' % out)
		else:
			return out

	def query(self, query, DB=None):
		"""given a SQL query, execute and return results as []"""
		if DB:
			query = "USE %s;\"%s\"" % (DB,query)
		else:
			query = "\"%s;\"" % query
		cmd = "echo %s | mysql -s mysql " % (query)
		if self.rootpw:
			cmd += "--password=%s" % (self.rootpw)
		ret,out = run(cmd)
		if not ret:
			return out
		else:
			log.error('problem executing query: %s' % query)
	def runFile(self, sqlfile, db, username=None, password=None):
		"""run file against mysql"""
		if not username:
			username = 'root'
		if not password:
			password = self.rootpw
		cmd = "mysql --user=%s" % (username)
		if password:
			cmd += " --password=%s" % (password)
		cmd += " %s < %s" % (db,sqlfile)
		log.debug('running runFile:%s' % (sqlfile))
		ret, out = run(cmd)
		if not ret:
			return out
		else:
			log.error('problem loading mysql file: %s' % (sqlfile))
			return False
	def getDatabases(self):
		"""return list of databases"""
		return self.query("show databases")
	def addDatabase(self, dbname):
		"""Add database, dbname is ''"""
		out = self.runAdmin('create %s' % (dbname))
		if out:
			log.error('error:%s' % out)
	def addUserAndPerms(self, user, password, dbname):
		"""add user with all perms to DB"""
		#TODO: make this more generalized, so we can give specific permissions if we want.
		q = "GRANT ALL PRIVILEGES ON %s.* TO %s@localhost IDENTIFIED BY '%s' " % (dbname,user,password)
		return self.query(q)

class MailmanADM(object):
	"""Manage mailman mailing lists."""

	def list(self, refresh=False):
		"""list mailing lists.
		2 matching mailing lists found:
		    Africa-list - [no description available]
		        Mailman - [no description available]"""
		lists = {}
		ret, out = run('list_lists')
		for l in out[1:]:
			name, desc = l.split(' - ')
			lists[name.strip()] = desc.strip()
		return lists

	def create(self, listemail, listadmin, password):
		"""create a new mailing list.
		newlist -u webdomain -e emaildomain listname listadmin-addr admin-password"""
		email, domain = listemail.split('@')
		if email not in self.list():
			log.info('adding mailman list: %s' % (email))
			cmd = "newlist -q -u %s -e %s %s %s '%s'" % (domain, domain, email, listadmin, password)
			run(cmd)


class PostgresMonitor(object):
	"""monitor postgres instances.
	options is a dict()
	keys: format below is key: description: default value.
		host: hostname to connect against: localhost
		user: username to connect with: use ~/.pgpass
		pass: password to connect with: use ~/.pgpass
	"""
	def __init__(self,options={}):
		self.enabled = True
		self.options = options
		self.check_postgres=os.path.join(classes.cifitlocation,'contrib','check_postgres','check_postgres.pl')
		if not os.path.exists(self.check_postgres):
			log.error('postgresMonitor not enabled.')
			self.enabled = False
	def run(self,action):
		if self.enabled:
			host = self.options.setdefault('host','localhost')
			cmd = "%s" % (self.check_postgres)
			if self.options.setdefault('host'):
				cmd += " -H %s" % self.options['host']
			if self.options.setdefault('user'):
				cmd += " -u %s" % self.options['user']
			if self.options.setdefault('pass'):
				cmd += " --dbpass=%s" % (self.options['pass'])
			cmd += " --action=%s" % action
			ret,output = run(cmd)
			if ret:
				#check_postgres returns 
				log.critical('PGMonitor:%s %s' % (action,output))
			else:
				ret = None
		else:
			ret = False
		return ret

class PostgresADM(object):
	"""Manage Postgres instances"""
	def __init__(self):
		self.monitor = PostgresMonitor()
	def psql(self,cmd):
		"""run cmd against psql"""
		ret,out = run("psql %s" % cmd)
		if ret:
			log.error('psql error: %s' % out)
			return False
		else:
			return out

	def getDatabases(self):
		"""get a list of databases"""
		return self.psql('--list')

	def addDatabase(self,name,options):
		"""add Database.

		createlang plpythonu
		createdb -E UTF8 dbname "description"
		"""

	def copyDatabase(self,source,dest):
		"""copy from source to dest a full working DB. will add if needed.
		issues:
			valid user/pass
			dest db must exist.
			veil must exist.
		example:
			 pg_dump -Fc -h joliet -U craig yuma_prod | pg_restore -Fc  --clean -d nightly_yuma -h localhost  -U craig
		"""

