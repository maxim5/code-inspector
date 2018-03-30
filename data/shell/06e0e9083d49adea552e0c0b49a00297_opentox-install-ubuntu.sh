#!/bin/bash
#Installation is tested on Debian Ubuntu 9.10
#Update the system

FAILED=0
STARTPATH=$PWD
ERRLOG=$PWD/'install_err.log'
INSTALLLOG=$PWD/'install_log.log'
DATE=$(date +%Y/%m/%d\ %H:%M:%S)
BRANCH=$1
GEMVERSION="1.3.5"
GITVERSION="1.6.5.2"
RAPTORVERSION="1.4.20"
RASQALVERSION="0.9.16"
RASQALVERSION2="0.9.15"
REDLANDVERSION="1.0.7"
REDBINDVERSION="1.0.7.1"

if [ "$BRANCH" = '' ]
then
  echo "Please enter: sudo ./[filename] [brunchtpy]"
  exit 1
fi
echo "================================================="
echo "Selected branch is: $BRANCH"
echo "================================================="
echo "Please enshure that the sudo package is installed"
echo "on your system. "
echo "On Ubuntu Linux sudo is installed by default."
echo "If you are unsure check with it 'sudo ls'"
echo "and installed it with 'apt-get install sudo'"
echo "and add your username with visudo."
echo "================================================="
echo "Some programs and the OpenTox webservices will be installed in the current folder."
echo "================================================="
echo -n "To continue installation press y: "
read answer
if [ "$answer" != "y" ]
then
  echo "exiting the script..."
  exit 1
fi

echo "opentox webservice install log - " $DATE > $INSTALLLOG
echo "opentox webservice install err log - " $DATE > $ERRLOG
echo "Installing: build-essential"
sudo apt-get install build-essential  | tee -a $INSTALLLOG

echo "Installing: ruby 1.8 with its dev files"
sudo apt-get install ruby ruby1.8-dev  | tee -a $INSTALLLOG

echo "Installing: gems rdoc rubygems libxml-parser-ruby1.8 libxml++2.6-dev libyaml-ruby libzlib-ruby sqlite3 libsqlite3-dev libsqlite3-ruby1.8 and rake"
sudo apt-get install gems rdoc rubygems libxml-parser-ruby1.8 libxml++2.6-dev libyaml-ruby libzlib-ruby rake sqlite3 libsqlite3-dev libsqlite3-ruby1.8 | tee -a $INSTALLLOG  

#RUBYGEMS
echo "Installing rubygems from source. This may take some time"
if [ ! -d $STARTPATH/rubygems-$GEMVERSION ]; 
then
	wget http://rubyforge.org/frs/download.php/60718/rubygems-$GEMVERSION.tgz >>$INSTALLLOG 2>>$ERRLOG 
	tar xzfv rubygems-$GEMVERSION.tgz  >>$INSTALLLOG 2>>$ERRLOG
	cd rubygems-$GEMVERSION  
	sudo ruby setup.rb  >>$INSTALLLOG 2>>$ERRLOG
	cd ..
	sudo rm rubygems-$GEMVERSION.tgz  
	CHECKGEM=`gem -v` 
	if [ "$CHECKGEM"  == "$GEMVERSION" ]
	then
		echo "Adding http://gems.github.com to ruby gem sources"
		sudo gem sources -a http://gems.github.com  >>$INSTALLLOG 2>>$ERRLOG

		echo "================================================="
		echo "Rubygems version $GEMVERSION successfully installed."
		echo "================================================="
	else
	    echo "Rubygems version $GEMVERSION installation failed."
	    FAILED=1
        exit $FAILED
	fi
else
	echo "rubygems-$GEMVERSION folder already exist. "
fi

echo "Installing packages: zlib1g-dev tcl curl perl ssh tcl tk8.5 libopenssl-ruby libgsl0-dev swig r-base rinruby"
sudo apt-get install zlib1g-dev tcl curl perl libopenssl-ruby libgsl0-dev r-base | tee -a $INSTALLLOG
sudo apt-get install ssh tcl tk8.5 | tee -a $INSTALLLOG
sudo apt-get install swig | tee -a $INSTALLLOG
sudo apt-get install postgresql-server-dev-8.4 | tee -a $INSTALLLOG


#echo "Installing gems jeweler sinatra emk-sinatra-url-for dm-core cehoffman-sinatra-respond_to rest-client rack-contrib thin cucumber datamapper data_objects do_sqlite3 rinruby"
sudo gem install jeweler  | tee -a $INSTALLLOG
sudo gem install sinatra | tee -a $INSTALLLOG
sudo gem install emk-sinatra-url-for  -s http://gems.github.com | tee -a $INSTALLLOG
sudo gem install dm-core  | tee -a $INSTALLLOG
sudo gem install sinatra-respond_to | tee -a $INSTALLLOG
sudo gem install rest-client | tee -a $INSTALLLOG
sudo gem install rack-contrib | tee -a $INSTALLLOG
sudo gem install thin | tee -a $INSTALLLOG
sudo gem install cucumber | tee -a $INSTALLLOG
sudo gem install datamapper | tee -a $INSTALLLOG
sudo gem install data_objects | tee -a $INSTALLLOG
sudo gem install do_sqlite3 | tee -a $INSTALLLOG
sudo gem install rinruby | tee -a $INSTALLLOG
sudo gem cleanup | tee -a $INSTALLLOG

echo "Installing LibRDF-ruby"
sudo apt-get install librdf0 librdf-ruby | tee -a $INSTALLLOG

#GIT
echo "Installing git from source"
echo "This could take a while..."
if [ ! -d $STARTPATH/git-$GITVERSION ];  
then
	wget http://www.kernel.org/pub/software/scm/git/git-$GITVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	tar xzfv git-$GITVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	cd git-$GITVERSION
	./configure   >>$INSTALLLOG 2>>$ERRLOG
	make   >>$INSTALLLOG 2>>$ERRLOG
	sudo make install   >>$INSTALLLOG 2>>$ERRLOG
	cd ..
	sudo rm git-$GITVERSION.tar.gz
	CHECKGIT=`git --version`
	if [ "$CHECKGIT"  == "git version $GITVERSION" ]
	then
		echo "================================================="
    		echo "Git version $GITVERSION successfully installed."
		echo "================================================="
	else
	    echo "Git version $GITVERSION installation failed."
	    FAILED=1
        exit $FAILED
	fi
else
	echo "git-$GITVERSION folder exists."
fi

#REDLAND
if [ ! -d $STARTPATH/redland ];
then
	echo "Making Redland folder."
	mkdir redland >>$INSTALLLOG 2>>$ERRLOG
	cd redland
	echo "Installing Redland raptor"
	if [ ! -d $STARTPATH/redland/raptor-$RAPTORVERSION ];
	then
		wget wget http://download.librdf.org/source/raptor-$RAPTORVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
		tar xzfv raptor-$RAPTORVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
		cd raptor-$RAPTORVERSION
		./configure  >>$INSTALLLOG 2>>$ERRLOG
		sudo make   >>$INSTALLLOG 2>>$ERRLOG
		sudo make install   >>$INSTALLLOG 2>>$ERRLOG
		cd ..
		sudo rm raptor-$RAPTORVERSION.tar.gz
		CHECKRAPTOR=`raptor-config --version`
		if [ "$CHECKRAPTOR"  == "$RAPTORVERSION" ]
		then
			echo "================================================="
			echo "Raptor version $RAPTORVERSION successfully installed."
			echo "================================================="
		else
		    echo "Raptor version $RAPTORVERSION installation failed."
		    FAILED=1
            exit $FAILED
		fi
	else
		echo "raptor-$RAPTORVERSION folder exists."
	fi

	echo "Installing Redland rasqal"
	wget wget http://download.librdf.org/source/rasqal-$RASQALVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	tar xzfv rasqal-$RASQALVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	cd rasqal-$RASQALVERSION
	./configure  >>$INSTALLLOG 2>>$ERRLOG
	sudo make   >>$INSTALLLOG 2>>$ERRLOG
	sudo make install   >>$INSTALLLOG 2>>$ERRLOG
	cd ..
	sudo rm rasqal-$RASQALVERSION.tar.gz
	CHECKRASQAL=`rasqal-config --version`
	if [ "$CHECKRASQAL"  == "$RASQALVERSION2" -o  "$CHECKRASQAL"  == "$RASQALVERSION" ]
	then
		echo "================================================="
		echo "Raptor version $RASQALVERSION2 or higher successfully installed."
		echo "================================================="
	else
	    echo "Raptor version $RASQALVERSION2 or higher installation failed."
	    FAILED=1
        exit $FAILED
	fi

	echo "Installing Redland redland"
	wget wget http://download.librdf.org/source/redland-$REDLANDVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	tar xzfv redland-$REDLANDVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	cd redland-$REDLANDVERSION
	./configure  >>$INSTALLLOG 2>>$ERRLOG
	sudo make   >>$INSTALLLOG 2>>$ERRLOG
	sudo make install   >>$INSTALLLOG 2>>$ERRLOG
	cd ..
	sudo rm redland-$REDLANDVERSION.tar.gz
	CHECKREDLAND=`redland-config --version`
	if [ "$CHECKREDLAND"  == "$REDLANDVERSION" ]
	then
		echo "================================================="
		echo "Redland version $REDLANDVERSION successfully installed."
		echo "================================================="
	else
	    echo "Redland version $REDLANDVERSION installation failed."
	    FAILED=1
        exit $FAILED
	fi


	echo "Installing Redland Bindings with ruby"
	wget http://download.librdf.org/source/redland-bindings-$REDBINDVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	tar xzfv redland-bindings-$REDBINDVERSION.tar.gz  >>$INSTALLLOG 2>>$ERRLOG
	cd redland-bindings-$REDBINDVERSION
	./configure --with-ruby  >>$INSTALLLOG 2>>$ERRLOG
	sudo make   >>$INSTALLLOG 2>>$ERRLOG
	sudo make install   >>$INSTALLLOG 2>>$ERRLOG
	cd ..
	sudo rm redland-bindings-$REDBINDVERSION.tar.gz
	cd ..
	#CHECKREDBIND=`??? --version`
	#if [ "$CHECKREDBIND"  == "$REDBINDVERSION" ]
	#then
	#	echo "================================================="
	#	echo "Redland Bindings version $REDBINDVERSION successfully installed."
	#	echo "================================================="
	#else
	#    echo "Redland Bindings version $REDBINDVERSION installation failed."
	#    FAILED=1
    #    exit $FAILED
	#fi
else
	echo "Redland folder exists."
fi

echo "Installing the opentox webservices"
mkdir webservices  
cd webservices  

echo "Install the opentox-ruby-api-wrapper"
echo "This could take a while..."
git clone git://github.com/helma/opentox-ruby-api-wrapper.git >>$INSTALLLOG 2>>$ERRLOG
cd opentox-ruby-api-wrapper
git checkout -b $BRANCH origin/$BRANCH >>$INSTALLLOG 2>>$ERRLOG
OTAPIVERSION=`cat VERSION`
sudo rake install  | tee -a $INSTALLLOG
cd ..
CHECKOTAPI=`gem list | grep "opentox-ruby-api-wrapper" | grep "$OTAPIVERSION"`
if [ ! "$CHECKOTAPI" = '' ]
then
	echo "================================================="
    	echo "opentox-ruby-api-wrapper ($OTAPIVERSION) successfully installed."
	echo "================================================="
else
	echo "opentox-ruby-api-wrapper ($OTAPIVERSION) installation failed."
	FAILED=1
    exit $FAILED
fi

echo "Installing the webservices: compound, dataset, algorithm, model, task, feature"
git clone git://github.com/helma/opentox-compound.git >>$INSTALLLOG 2>>$ERRLOG
git clone git://github.com/helma/opentox-dataset.git >>$INSTALLLOG 2>>$ERRLOG
git clone git://github.com/helma/opentox-algorithm.git >>$INSTALLLOG 2>>$ERRLOG
git clone git://github.com/helma/opentox-model.git >>$INSTALLLOG 2>>$ERRLOG
git clone git://github.com/helma/opentox-task.git >>$INSTALLLOG 2>>$ERRLOG
git clone git://github.com/helma/opentox-feature.git >>$INSTALLLOG 2>>$ERRLOG

cd opentox-compound  
git checkout -b $BRANCH origin/$BRANCH >>$INSTALLLOG 2>>$ERRLOG 
cd ../opentox-dataset  
git checkout -b $BRANCH origin/$BRANCH >>$INSTALLLOG 2>>$ERRLOG
cd ../opentox-algorithm  
git checkout -b $BRANCH origin/$BRANCH >>$INSTALLLOG 2>>$ERRLOG
cd ../opentox-model  
git checkout -b $BRANCH origin/$BRANCH >>$INSTALLLOG 2>>$ERRLOG
cd ../opentox-task
git checkout -b $BRANCH origin/$BRANCH >>$INSTALLLOG 2>>$ERRLOG
cd ../opentox-task
git checkout -b development origin/development >>$INSTALLLOG 2>>$ERRLOG
cd ..  

#edit /home/[username]/.opentox/config/test.yaml set :base_dir: /home/[username]/webservices

echo "Installing the tests"                                                   
git clone git://github.com/helma/opentox-test.git  >>$INSTALLLOG 2>>$ERRLOG   
cd opentox-test                                                               
git checkout -b $BRANCH origin/$BRANCH  >>$INSTALLLOG 2>>$ERRLOG       

echo "Installing openbabel"
cd ../opentox-compound
sudo rake openbabel:install | tee -a $INSTALLLOG
sudo ldconfig >>$INSTALLLOG 2>>$ERRLOG

ln -s /usr/local/lib/libopenbabel.so.3 /usr/lib/libopenbabel.so.3 >> $INSTALLLOG 2>>$ERR
#VERSION=` --version`
#if [ "$VERSION"  == "$RAPTORVERSION" ]
#then
#	echo "================================================="
#    	echo "Raptor version $RAPTORVERSION successfully installed."
#	echo "================================================="
#else
#    echo "Raptor version $RAPTORVERSION installation failed."
#    FAILED=1
#    exit $FAILED
#fi


#check /webservices/opentox-algorithm/fminer.rb for 1,0/ture,false bug
BUGCHECK1=`grep "@@fminer.AddActivity(true, id)" $STARTPATH/webservices/opentox-algorithm/fminer.rb`
BUGCHECK2=`grep "@@fminer.AddActivity(false, id)" $STARTPATH/webservices/opentox-algorithm/fminer.rb`
if [ -z "$BUGCHECK1$BUGCHECK2" ]
then
        echo "fminer.rb is correct."
else
        sed -i 's/@@fminer.AddActivity(true, id)/@@fminer.AddActivity(1, id)/g' $STARTPATH/webservices/opentox-algorithm/fminer.rb
        sed -i 's/@@fminer.AddActivity(false, id)/@@fminer.AddActivity(0, id)/g' $STARTPATH/webservices/opentox-algorithm/fminer.rb
        echo "fminer.rb updated."
fi

#todo: configure libfminer Makefile (location of ruby.h)
#-> fixed by using davor's repository


cd ../opentox-algorithm
echo "Installing fminer"
echo "This could take a while..."
sudo updatedb
sudo rake fminer:install | tee -a $INSTALLLOG
cd ..
FMINERVERSION=`ls $STARTPATH/webservices/opentox-algorithm/libfminer | grep "fminer.so"`
if [ "$FMINERVERSION"  == "fminer.so" ]
then
	echo "================================================="
    	echo "Fminer successfully installed."
	echo "================================================="
else
    echo "Fminer installation failed."
    FAILED=1
    exit $FAILED
fi

if [ $FAILED == 0 ]
then
	#get username
	echo "Please enter username:"
	read USERNAME

	#change rights from root to user
	sudo chown -R $USERNAME: $STARTPATH/webservices/
	sudo chown -R $USERNAME: ~/.opentox/
fi

if [ "$FAILED" == "1" ]
then
	echo "================================================="
    	echo "Installation script failed!"
	echo "================================================="
    	exit 1
else
echo "================================================="
echo "opentox-install-script is completed."
echo "================================================="
echo "Configuration:"
echo "Edit the settings in $HOME/.opentox/config/{development|production|test}.yaml for your environment."
echo "================================================="
echo "Start the webservices local:"
echo "cd webservices/opentox-test/"
echo "rake opentox:services:start"
echo "================================================="
echo "Test local webservices:"
echo "rake features"
echo "================================================="
fi
exit 0
