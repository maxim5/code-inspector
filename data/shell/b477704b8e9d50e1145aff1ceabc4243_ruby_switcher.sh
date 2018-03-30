#
#  Manage and use multiple Ruby interpreters installed in a globally-accessible
#  location.  Based on a few simple rules/assumptions:
#    1. Ruby interpreters should globally accessible - default: /opt/ruby
#    2. Global instance of RubyGems - default location
#    3. Single gem repository per Ruby major version
#    4. Use of simple commands to install, uninstall, and use Ruby versions
#
#  Usage:
#    - Place script in an accessible location - i.e. /usr/local/bin.
#    - Modify as needed
#    - echo "source /path/to/script/ruby_switcher.sh" >> ~/.bashrc
#
#  Inspired by the ruby_switcher.sh script shared by the nice guys at Relevance.
#    http://thinkrelevance.com
#    http://github.com/relevance/etc/blob/master/bash/ruby_switcher.sh
#


export ORIGINAL_PATH=$PATH
export RUBY_BASEDIR=/opt/ruby


#
#  General functions
#
function install_ruby_from_source {
  local ruby_major=$1
  local ruby_minor=$2
  local patch_level=$3
  local ruby_version="ruby-$1.$2-p$patch_level"
  local url="ftp://ftp.ruby-lang.org/pub/ruby/$ruby_major/$ruby_version.tar.gz"

  mkdir -p /tmp && mkdir -p $RUBY_BASEDIR/$ruby_major && rm -rf $RUBY_BASEDIR/$ruby_major/$ruby_version &&
  pushd /tmp

  if [ ! -e "$ruby_version.tar.gz" ]
  then
    curl --silent -L -O $url
  fi

  if [ -e "$ruby_version" ]
  then
    rm -rf $ruby_version
  fi

  tar xzf $ruby_version.tar.gz &&
  cd $ruby_version &&
  ./configure --prefix=$RUBY_BASEDIR/$ruby_major/$ruby_version --enable-shared &&
  make && make install && cd /tmp &&
  mkdir -p $RUBY_BASEDIR/$ruby_major/gems &&
  rm -rf $ruby_version.tar.gz $ruby_version
}

function use_ruby {
  local ruby_major=$1
  local ruby_minor=$2
  local patch_level=$3
  local ruby_version="ruby-$1.$2-p$patch_level"

  if [ -e "$RUBY_BASEDIR/$ruby_major/$ruby_version" ]
  then
    export MY_RUBY_HOME=$RUBY_BASEDIR/$ruby_major/$ruby_version
    export GEM_HOME=$RUBY_BASEDIR/$ruby_major/gems
    export GEM_PATH=$GEM_HOME
    update_path  
  else
    echo "Ruby $ruby_major.$ruby_minor is not installed on this system."
  fi

}

function update_path {
  export RUBYGEMS_HOME=$RUBY_BASEDIR/rubygems 
  export PATH=$GEM_HOME/bin:$MY_RUBY_HOME/bin:$RUBYGEMS_HOME/bin:$ORIGINAL_PATH
  export RUBY_VERSION="$(ruby -v | colrm 11)"
  display_ruby_version
}

function install_rubygems {
  if [ ! -e "$RUBYGEMS_HOME" ]
  then
    local rubygems_version="rubygems-1.3.5"
    local rubygems_url="http://rubyforge.org/frs/download.php/60718/rubygems-1.3.5.tgz"

    mkdir -p /tmp &&
    pushd /tmp &&
    curl --silent -L -O $rubygems_url &&
    tar xzf $rubygems_version.tgz &&
    cd $rubygems_version &&
    ruby setup.rb && cd /tmp &&
    rm -rf $rubygems_version.tgz $rubygems_version &&
    popd
  fi
}

function install_rake {
  local rake_count=`gem list --local | grep rake | wc -l`
  if [ "$rake_count" -eq "0" ]
  then
    gem install -q --no-ri --no-rdoc rake
  fi
}

function display_ruby_version {
 if [[ $SHELL =~ "bash" ]]; then
   echo "Using `ruby -v`"
 fi
 # On ZSH, show it on the right PS1
 export RPS1=$RUBY_VERSION
}

function uninstall_ruby {
  local ruby_major=$1
  local ruby_minor=$2
  local patch_level=$3
  local ruby_version="ruby-$1.$2-p$patch_level"

  rm -rf $RUBY_BASEDIR/$ruby_major/$ruby_version

  local ruby_count=`find $RUBY_BASEDIR/$ruby_major -maxdepth 1 -type d | wc -l`
  if [ "$ruby_count" -eq "0" ]
  then
    rm -rf $RUBY_BASEDIR/$ruby_major
  fi
}



#
#  Ruby 1.8.6 functions
#
function install_ruby_186 {
  install_ruby_from_source "1.8" "6" "369" && use_ruby_186 && 
  install_rubygems && install_rake && popd
}

function uninstall_ruby_186 {
  uninstall_ruby "1.8" "6" "369"  
}

function use_ruby_186 {
  use_ruby "1.8" "6" "369"
}



#
#  Ruby 1.8.7 functions
#
function install_ruby_187 {
  install_ruby_from_source "1.8" "7" "174" && 
  use_ruby_187 && install_rubygems && install_rake && popd
}

function uninstall_ruby_187 {
  uninstall_ruby "1.8" "7" "174"  
}

function use_ruby_187 {
  use_ruby "1.8" "7" "174"
}



#
#  Ruby 1.9.1 functions
#
function install_ruby_191 {
  install_ruby_from_source "1.9" "1" "243" && use_ruby_191 && 
  gem update --system && install_rake && popd
}

function uninstall_ruby_191 {
  uninstall_ruby "1.9" "1" "243"  
}

function use_ruby_191 {
  use_ruby "1.9" "1" "243"
}



#
#  Ruby 1.9.2 functions
#
function install_ruby_192 {
  install_ruby_from_source "1.9" "2" "review1" && use_ruby_192 && 
  gem update --system && install_rake && popd
}

function uninstall_ruby_192 {
  uninstall_ruby "1.9" "2" "review1"  
}

function use_ruby_192 {
  use_ruby "1.9" "2" "review1"
}



use_ruby_187
