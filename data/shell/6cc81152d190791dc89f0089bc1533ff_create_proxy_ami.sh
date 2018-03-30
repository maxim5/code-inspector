#!/bin/bash
#
# ec2ubuntu-build-ami
#
# This script builds, bundles, and uploads an Ubuntu base install AMI
# for Amazon EC2.  This can be run on any of the following Fedora Core
# AMIs, depending on what type of new Ubuntu AMI you wish to create:
#
#   32-bit, 2.6.21 kernel: ami-f51aff9c
#   32-bit, 2.6.16 kernel: ami-20b65349
#   64-bit, 2.6.16 kernel: ami-36ff1a5f
#   64-bit, 2.6.21 kernel: ami-f21aff9b
#
# Command line options:
#
#    --bucket NAME       - REQUIRED
#    --prefix PREFIX     - Defaults to a reasonable manifest name.
#    --user ID           - Defaults to $AWS_USER_ID
#    --access-key ID     - Defaults to $AWS_access_key
#    --secret-key ID     - Defaults to $AWS_SECRET_access_key
#    --private-key PATH  - Defaults to $EC2_PRIVATE_KEY
#    --cert PATH         - Defaults to $EC2_CERT
#    --release VERSION   - One of: 6.06 6.10 7.04 7.10 8.04
#    --codename NAME     - Only needed if release not recognized.
#    --timezone ZONE     - Defaults to US/Pacific
#    --lang LANG         - Defaults to en_US.UTF-8
#    --size MB           - Root partition size in megabytes
#    --package NAME      - Additional Ubuntu package to install
#    --script FILE       - External script to run before bundle
#    --desktop nx        - Takes longer and makes AMI much bigger
#
#    It is ok to specify multiple --package and --script options.
#    You can also use "--desktop yes" to install the desktop packages
#    without the NX software.
#
# For Amazon EC2 AMIs built using this script:
#
#   http://alestic.com
#
# For updated versions of this script:
#
#   http://code.google.com/p/ec2ubuntu/
#
# For general Ubuntu on EC2 support:
#
#   http://groups.google.com/group/ec2ubuntu
#
# History:
#
#   2008-05-16 Eric Hammond <ehammond@thinksome.com>
#   - Run the instance user-data if it looks like a script (starts with #!)
#   - Wait for the network (DHCP) to come up before grabbing instance data.
#
#   2008-05-14 Eric Hammond <ehammond@thinksome.com>
#   - Create new ssh host keys on first boot.
#   - Disable apparmor as we don't have the kernel module installed yet.
#   - Don't claim every AMI was built by Eric Hammond in /etc/motd
#   - Create /tmp earlier in the boot process to avoid warnings.
#   - Implemented following suggestions from Hans Omli:
#   - Disable useless CMOS hwclock at boot to avoid error and save seconds.
#   - Avoid grep warning about missing authorized_keys file on first boot.
#
#   2008-05-13 Thomas Shealy <thomas.shealy@gmail.com>
#   - Add --retry to ec2-upload-bundle.
#
#   2008-05-12 Thomas Shealy <thomas.shealy@gmail.com>
#   - Support 64-bit desktop AMIs.
#   - Upgrade to NX 3.2.
#
#   2008-04-29 Eric Hammond <ehammond@thinksome.com>
#   - Support command line options with reasonable defaults.
#   - Support building Ubuntu 8.04 Hardy Heron.
#
#   2008-04-22 Vincent Desjardins
#   - Add a variable to override the default size of the image created
#   - Add a optional call to an external script before bundling the AMI
#
#   2008-04-19 Eric Hammond <ehammond@thinksome.com>
#   - Support 2.6.21 kernel with 64-bit builds.
#
#   2008-04-18 Eric Hammond <ehammond@thinksome.com>
#   - Fix performance problem with 2.6.21 initrd (with killall nash-hotplug).
#
#   2008-04-11 Eric Hammond <ehammond@thinksome.com>
#   - Install kernel modules for both 2.6.16-xenU and 2.6.21.7-2.fc8xen
#
#   2008-04-05 Eric Hammond <ehammond@thinksome.com>
#   - Add support for desktop build with NoMachine NX Free.
#
#   2008-04-03 Eric Hammond <ehammond@thinksome.com>
#   - Upgrade to latest AMI tools 1.3-20041 20071010 (obviates several patches)
#   - Add new Ubuntu patches for new version of AMI tools
#   - Switch from "uname -i" to "uname -m" to enable building on Ubuntu 64-bit
#   - Merge Dapper, Edgy, Feisty, Gutsy, Hardy scripts (Hardy doesn't work yet)
#
#   2008-03-13 Eric Hammond <ehammond@thinksome.com>
#   - Prevent apt-get from running newly installed daemons
#
#   2008-03-09 Eric Hammond <ehammond@thinksome.com>
#   - Upgrade to kernel modules 2.6.16.60
#   - Upgrade fuse kernel module to 2.7.3
#   - Upgrade to latest AMI tools 1.3-19365 (obviates one patch)
#
#   2008-02-05 Eric Hammond <ehammond@thinksome.com>
#   - Patch AMI tools to work with new Ruby 1.8.6
#
#   2008-02-03 Eric Hammond <ehammond@thinksome.com>
#   - Install rsync without lutimes support (as it's not in the EC2 kernel)
#
#   2008-01-17 Eric Hammond <ehammond@thinksome.com>
#   - Upgrade to debootstrap 1.0.8
#
#   2007-12-25 Eric Hammond <ehammond@thinksome.com>
#   - Install fuse kernel module (32-bit)
#   - Upgrade to debootstrap 1.0.7
#
#   2007-12-02 Eric Hammond <ehammond@thinksome.com>
#   - Use architecture "amd64" instead of "i386" for debootstrap on 64-bit
#   - Add ia32-libs compatability package for 64-bit
#
#   2007-12-01 Eric Hammond <ehammond@thinksome.com>
#   - Add support for building on 64-bit kernel (large, extra large instances)
#
#   2007-11-23 Eric Hammond <ehammond@thinksome.com>
#   - ssh credentials retrieved from instance parameters or ephemeral storage.
#   - Patch ec2-unbundle to work on Ubuntu
#   - Also add locale to /etc/default/locale
#
#   2007-11-22 Eric Hammond <ehammond@thinksome.com>
#   - Upgrade Ubuntu AMI tools patch to match new AMI tools source.
#   - Install ca-certificates to better support ec2-upload-bundle per:
#     http://developer.amazonwebservices.com/connect/thread.jspa?threadID=16543&tstart=0
#   - ec2-bundle-vol excludes /etc/udev/rules.d/70-persistent-net.rules
#     so that the network works on a rebundled instance, per:
#   http://developer.amazonwebservices.com/connect/message.jspa?messageID=70873
#
#   2007-11-18 Eric Hammond <ehammond@thinksome.com>
#   - Original put together based on code, tricks, and fixes from many
#     others.
#

export AWS_USER_ID=161964561164
export AWS_access_key=1XCTNEK1CC5BQPA3EE02
export ACCESS_KEY=$AWS_access_key
export AWS_SECRET_ACCESS_KEY=Q2qJHP0S2iOKikn9glB+KZcF/aYf4huS/GdHvqEZ
export SECRET_ACCESS_KEY=$AWS_SECRET_ACCESS_KEY
export bucket=a-instances
export prefix=ubuntu-proxy-base-$(date +%Y%m%d)

packages="ubuntu-standard openssh-server rsync ruby openssl curl ca-certificates libopenssl-ruby1.8 patch alien"

while [ $# -gt 0 ]; do
  case $1 in
    --release)     release=$2;                          shift 2 ;;
    --codename)    codename=$2;                         shift 2 ;;
    --tag)         tag=$2;                              shift 2 ;;
    --bucket)      bucket=$2;                           shift 2 ;;
    --prefix)      prefix=$2;                           shift 2 ;;
    --user)        AWS_USER_ID=$2;                      shift 2 ;;
    --access-key)  AWS_access_key=$2;                shift 2 ;;
    --secret-key)  AWS_SECRET_ACCESS_KEY=$2;            shift 2 ;;
    --private-key) EC2_PRIVATE_KEY=$2;                  shift 2 ;;
    --cert)        EC2_CERT=$2;                         shift 2 ;;
    --timezone)    timezone=$2;                         shift 2 ;;
    --lang)        LANG=$2;                             shift 2 ;;
    --size)        size=$2;                             shift 2 ;;
    --script)      scripts="$scripts $2";               shift 2 ;;
    --package)     packages="$packages $2";             shift 2 ;;
    --desktop)     desktop=$2
                   packages="$packages ubuntu-desktop user-setup"
                                                        shift 2 ;;
    *)             echo "$0: Unrecognized option: $1" >&2; exit 1;
  esac
done

true ${release:=8.04}
if [ "$codename" = "" ]; then
  case $release in
    6.06)  codename=dapper ;;
    6.10)  codename=edgy   ;;
    7.04)  codename=feisty ;;
    7.10)  codename=gutsy  ;;
    8.04)  codename=hardy  ;;
    *)     echo "$0: Unrecognized release: $release" >&2; exit 1;
  esac
fi

# Required and default parameters
true ${AWS_USER_ID:?} ${AWS_access_key:?} ${AWS_SECRET_ACCESS_KEY:?} \
     ${bucket:?} \
     ${EC2_CERT:=$(echo /mnt/cert-*.pem)} \
     ${EC2_PRIVATE_KEY:=$(echo /mnt/pk-*.pem)} \
     ${timezone:=US/Pacific} ${LANG:=en_US.UTF-8} \
     ${tag:=custom} ${prefix:=ubuntu-$release-$codename-$tag-$(date +%Y%m%d)}

if [ "$codename" == "dapper" ]; then
  true ${size:=3072}
else
  true ${size:=4096}
fi

echo Building AMI for Ubuntu $release $codename
echo timezone:     $timezone
echo LANG:         $LANG
echo Image size:   ${size} MB
echo Uploading to: $bucket/$prefix

set -e
set -x

# The good stuff starts here.

mkdir -p /mnt/build
cd /mnt/build
mkdir -p ubuntu

if which apt-get >/dev/null 2>/dev/null; then
  # Ubuntu / Debian
  apt-get install -y binutils
else
  # Fedora Core / Red Hat / CentOS
  yum install -y binutils
fi

if [ $(uname -m) = 'x86_64' ]; then
  modules="http://s3.amazonaws.com/ec2-downloads/ec2-modules-2.6.16.33-xenU-x86_64.tgz http://alestic-downloads.s3.amazonaws.com/ec2-kernel-modules-2.6.21.x86_64.tar.gz"
  bundlearch="x86_64"
  bsarch="amd64"
  export notlongext="64"
else
  # http://groups.google.com/group/ec2ubuntu/web/compiling-kernel-modules-from-source-for-amazon-ec2
  modules="http://alestic-downloads.s3.amazonaws.com/ec2-kernel-modules-2.6.16-xenU.tgz http://alestic-downloads.s3.amazonaws.com/ec2-kernel-modules-2.6.21.7-2.fc8xen.tar.gz"
  bundlearch="i386"
  bsarch="i386"
  export notlongext=""
fi

if [ "$desktop" = "nx" ]; then
  notlongext="$notlongext-desktop"
fi

# Don't launch daemons on apt-get install
mkdir -p ubuntu/usr/sbin/
cat << EOF > ubuntu/usr/sbin/policy-rc.d
#!/bin/sh
exit 101
EOF
chmod 755 ubuntu/usr/sbin/policy-rc.d

# Bootstrap Ubuntu
BSURL=http://archive.ubuntu.com/ubuntu/pool/main/d/debootstrap
BSVER=debootstrap_1.0.8
BSDIR=debootstrap
curl -s $BSURL/$BSVER.tar.gz |
  tar xz
curl -s $BSURL/${BSVER}_all.deb > /tmp/${BSVER}_all.deb
ar p /tmp/${BSVER}_all.deb data.tar.gz |
  tar xvzOf - ./usr/share/debootstrap/devices.tar.gz > $BSDIR/devices.tar.gz
if [ "$codename" = "hardy" ]; then
  ln -s gutsy $BSDIR/scripts/ubuntu/$codename
fi
ln -s ubuntu/$codename $BSDIR/scripts/$codename
export DEBOOTSTRAP_DIR=$BSDIR
perl -pi.bak -e 'print "set -x\n" if $.==2' $BSDIR/debootstrap
$BSDIR/debootstrap --arch $bsarch $codename ubuntu http://us.archive.ubuntu.com/ubuntu

chroot ubuntu mount -t proc none /proc
chroot ubuntu mkdir -p /dev/pts
chroot ubuntu mount -t devpts none /dev/pts
chroot ubuntu mknod --mode 666 /dev/ptmx c 5 2

chroot ubuntu apt-get -f install -y

# Change these to your locale and timezone
chroot ubuntu localedef -i en_US -c -f UTF-8 en_US.UTF-8
echo $timezone >ubuntu/etc/timezone
echo "LANG=\"$LANG\""  >ubuntu/etc/default/locale
/bin/cp -f ubuntu/usr/share/zoneinfo/$timezone ubuntu/etc/localtime

# Basic sources.list
mv ubuntu/etc/apt/sources.list ubuntu/etc/apt/sources.list.orig || true
cat <<EOF >ubuntu/etc/apt/sources.list
deb http://us.archive.ubuntu.com/ubuntu $codename main restricted universe multiverse
deb-src http://us.archive.ubuntu.com/ubuntu $codename main restricted universe multiverse

deb http://us.archive.ubuntu.com/ubuntu $codename-updates main restricted universe multiverse
deb-src http://us.archive.ubuntu.com/ubuntu $codename-updates main restricted universe multiverse

deb http://security.ubuntu.com/ubuntu $codename-security main restricted universe multiverse
deb-src http://security.ubuntu.com/ubuntu $codename-security main restricted universe multiverse
EOF

# Update package list
chroot ubuntu apt-get update

# Architecture/release specific instructions
if [ "$bundlearch" = "i386" ]; then
  if [ "$codename" = "edgy"   -o \
       "$codename" = "feisty" -o \
       "$codename" = "gutsy" ]; then
    chroot ubuntu apt-get install -y libc6-xen
  fi
  if [ "$codename" = "hardy" ]; then
    # tls seems to have a bug on hardy: perl -e 'glob("xxx*")'
    mv ubuntu/lib/tls ubuntu/lib/tls.disabled
  fi
else
  chroot ubuntu apt-get install -y ia32-libs
fi

# MAKEDEV is expected in /dev by some packages.
ln -s /sbin/MAKEDEV ubuntu/dev/MAKEDEV

# Upgrade/install packages
chroot ubuntu apt-get -y upgrade
chroot ubuntu apt-get install -y $packages

# EC2 kernel modules
for module in $modules; do
  curl -s $module | tar xzC ubuntu
done
chroot ubuntu depmod -a

# Xen expects a single tty1
/bin/rm -f ubuntu/etc/event.d/tty[2-6]

# Security
chroot ubuntu shadowconfig on
chroot ubuntu passwd -d root

# Basic networking
cat <<'EOF' >ubuntu/etc/network/interfaces
auto lo
iface lo inet loopback

auto eth0
iface eth0 inet dhcp
EOF

cat <<'EOF' >ubuntu/etc/hosts
127.0.0.1 localhost.localdomain localhost

# The following lines are desirable for IPv6 capable hosts
::1 ip6-localhost ip6-loopback
fe00::0 ip6-localnet
ff00::0 ip6-mcastprefix
ff02::1 ip6-allnodes
ff02::2 ip6-allrouters
ff02::3 ip6-allhosts
EOF

cat <<'EOF' >>ubuntu/etc/ssh/sshd_config
UseDNS no
EOF

# Fedora 6/8 initrd starts nash-hotplug which uses 100% CPU and prevents
# udev from starting, not to mention slowing down everything else.
cat <<EOF >ubuntu/etc/init.d/ec2-killall-nash-hotplug
#!/bin/sh
/usr/bin/killall nash-hotplug
EOF
chmod 755 ubuntu/etc/init.d/ec2-killall-nash-hotplug
ln -s ../init.d/ec2-killall-nash-hotplug ubuntu/etc/rcS.d/S00ec2-killall-nash-hotplug

# motd
cat <<EOF >ubuntu/etc/rc.local
#!/bin/sh -e

# Get ssh credentials from instance parameters or ephemeral storage.
/usr/local/sbin/ec2-get-credentials

# If instance user-data starts with "#!" then run it on first boot.
/usr/local/sbin/ec2-run-user-data
EOF

# Script to run user-data if it looks like a script
curl -s -o ubuntu/usr/local/sbin/ec2-run-user-data \
  http://ec2ubuntu.googlecode.com/svn/trunk/bin/ec2-run-user-data
chmod 755 ubuntu/usr/local/sbin/ec2-run-user-data

# ec2-get-credentials
cat <<'EOF' >ubuntu/usr/local/sbin/ec2-get-credentials
#!/bin/bash
# Retrieve the ssh credentials and add to authorized_keys file.
# Based on /usr/local/sbin/ec2-get-credentials from ami-20b65349
public_key_url=http://169.254.169.254/1.0/meta-data/public-keys/0/openssh-key
public_key_file=/tmp/openssh_id.pub
public_key_ephemeral=/mnt/openssh_id.pub
authorized_keys=/root/.ssh/authorized_keys
test -d /root/.ssh || mkdir -p -m 700 /root/.ssh
perl -MIO::Socket::INET -e '
 until(new IO::Socket::INET("169.254.169.254:80")){print"Waiting for network...\n";sleep 1}
' | logger -t "ec2"
curl --retry 3 --silent --fail -o $public_key_file $public_key_url
if [ $? -eq 0 -a -e $public_key_file ] ; then
  if ! grep -s -q -f $public_key_file $authorized_keys
  then
    cat $public_key_file >> $authorized_keys
    echo "New ssh key added to $authorized_keys from $public_key_url" |
      logger -t "ec2"
  fi
  chmod 600 $authorized_keys
  rm -f $public_key_file
elif [ -e $public_key_ephemeral ] ; then
  if ! grep -s -q -f $public_key_ephemeral $authorized_keys
  then 
    cat $public_key_ephemeral >> $authorized_keys
    echo "New ssh key added to $authorized_keys from $public_key_ephemeral" |
      logger -t "ec2"
  fi
  chmod 600 $authorized_keys
  chmod 600 $public_key_ephemeral
fi
EOF
chmod 755 ubuntu/usr/local/sbin/ec2-get-credentials

# Install and patch Amazon AMI tools
curl -s http://s3.amazonaws.com/ec2-downloads/ec2-ami-tools.noarch.rpm \
  > ubuntu/tmp/ec2-ami-tools.noarch.rpm
chroot ubuntu alien -i --scripts /tmp/ec2-ami-tools.noarch.rpm

ln -s /usr/lib/site_ruby/aes ubuntu/usr/local/lib/site_ruby/1.8/aes
ln -s /usr/lib/site_ruby/ec2 ubuntu/usr/local/lib/site_ruby/1.8/ec2

chroot ubuntu patch -d /usr/lib/site_ruby/aes/amiutil <<'EOF'
--- /usr/lib/site_ruby/aes/amiutil/http.rb.orig	2008-04-02 22:27:01.000000000 -0400
+++ /usr/lib/site_ruby/aes/amiutil/http.rb	2008-04-02 22:28:05.000000000 -0400
@@ -58,7 +58,7 @@
     tf.close(false)
     
     begin
-      cmd_line = "curl -f  #{curl_arguments} #{url} 2> #{tf.path} | tee #{path} | openssl sha1; exit ${PIPESTATUS[0]}"
+      cmd_line = "/bin/bash -c 'curl -f  #{curl_arguments} #{url} 2> #{tf.path} | tee #{path} | openssl sha1; exit ${PIPESTATUS[0]}'"
       calculated_digest = IO.popen( cmd_line ) { |io| io.readline.chomp }
       
       unless $?.exitstatus == 0
--- /usr/lib/site_ruby/aes/amiutil/bundlevol.rb.orig	2008-04-03 00:57:26.000000000 -0400
+++ /usr/lib/site_ruby/aes/amiutil/bundlevol.rb	2008-04-03 00:58:42.000000000 -0400
@@ -148,6 +148,10 @@
   if image_file.index( volume ) == 0
     exclude << image_file
   end
+
+  # UGLY HACK for Ubuntu: Don't save the MAC address as that prevents
+  # networking from working on rebundled AMIs
+  exclude << "/etc/udev/rules.d/70-persistent-net.rules"
   
   # If we are inheriting instance data but can't access it we want to fail early
   if p.inherit && !AES::AmiUtils::InstanceData.new.instance_data_accessible
EOF

chroot ubuntu patch -d /usr/lib/site_ruby/ec2/platform/base <<'EOF'
--- /usr/lib/site_ruby/ec2/platform/base/pipeline.rb.orig	2008-04-02 19:41:54.000000000 -0700
+++ /usr/lib/site_ruby/ec2/platform/base/pipeline.rb	2008-04-02 19:47:17.000000000 -0700
@@ -105,7 +105,8 @@
           @results = []
           create_tempfiles
           
-          invocation = command
+          # UGLY HACK for Ubuntu: Assumes no single quotes (') in command.
+          invocation = "/bin/bash -c '" + command + "'"
           
           # Execute the pipeline invocation
           STDERR.puts("Pipeline.execute: command = [#{invocation}]") if verbose
EOF

cat <<EOF >ubuntu/etc/init.d/ec2-mkdir-tmp
#!/bin/sh
# Create /tmp if missing (as it's nice to bundle without it).
test -d /tmp || mkdir /tmp
chmod 01777 /tmp
EOF
chmod +x ubuntu/etc/init.d/ec2-mkdir-tmp
ln -s ../init.d/ec2-mkdir-tmp ubuntu/etc/rcS.d/S36ec2-mkdir-tmp


# Generate new ssh host keys on first boot
# Otherwise everybody knows our secret host key which makes it non-secret
cat <<EOF >ubuntu/etc/init.d/ec2-ssh-host-key-gen
#!/bin/sh
rm -f /etc/ssh/ssh_host_*_key*
ssh-keygen -f /etc/ssh/ssh_host_rsa_key -t rsa -C 'host' -N '' | logger -t \$0
ssh-keygen -f /etc/ssh/ssh_host_dsa_key -t dsa -C 'host' -N '' | logger -t \$0
rm -f /etc/rcS.d/S50ec2-ssh-host-key-gen
EOF
chmod +x ubuntu/etc/init.d/ec2-ssh-host-key-gen
ln -s ../init.d/ec2-ssh-host-key-gen ubuntu/etc/rcS.d/S50ec2-ssh-host-key-gen

# Inside Xen, CMOS clock is irrelevant, so save seconds at boot
/bin/rm -f ubuntu/etc/rc?.d/*hwclock*

# We're missing the apparmor kernel module for now so avoid boot warnings
/bin/rm -f ubuntu/etc/rc?.d/*apparmor

# Release-specific
if [ "$codename" = "gutsy" ]; then
  # Install a copy of rsync with no lutimes support, as the Amazon EC2
  # kernel does not support this newer feature to change symlink timestamps.
  curl -s -o ubuntu/tmp/rsync_2.6.9-5ubuntu1_$bsarch.deb \
    https://level22.s3.amazonaws.com/20080203-rsync-no-lutimes/rsync_2.6.9-5ubuntu1_$bsarch.deb
  chroot ubuntu dpkg --install /tmp/rsync_2.6.9-5ubuntu1_$bsarch.deb
fi

# Are we installing the desktop with NoMachine NX?
if [ "$desktop" = "nx" ]; then
  if [ "$bundlearch" = "i386" ]; then
    nxclient_pkg="nxclient_3.2.0-9_i386.deb"
    nxnode_pkg="nxnode_3.2.0-5_i386.deb"
    nxserver_pkg="nxserver_3.2.0-7_i386.deb"
  else
    nxclient_pkg="nxclient_3.2.0-9_x86_64.deb"
    nxnode_pkg="nxnode_3.2.0-5_x86_64.deb"
    nxserver_pkg="nxserver_3.2.0-7_x86_64.deb"
  fi

  curl -o ubuntu/tmp/$nxclient_pkg \
    http://64.34.161.181/download/3.2.0/Linux/$nxclient_pkg
  curl -o ubuntu/tmp/$nxnode_pkg \
    http://64.34.161.181/download/3.2.0/Linux/$nxnode_pkg
  curl -o ubuntu/tmp/$nxserver_pkg \
    http://64.34.161.181/download/3.2.0/Linux/FE/$nxserver_pkg

  chroot ubuntu dpkg -i /tmp/$nxclient_pkg
  chroot ubuntu dpkg -i /tmp/$nxnode_pkg
  chroot ubuntu dpkg -i /tmp/$nxserver_pkg
fi

# Call external script if requested
for script in $scripts; do
  if [ -f "$script" ]; then
    if [ -e "$script" ]; then
      "$script"
    else
      /bin/bash "$script"
    fi
  fi
done

############################## CUSTOM INSTALLS ####################################
# BUILD AMI HERE
###################################################################################

# cleanup
if [ "$codename" != "dapper" ]; then
  chroot ubuntu apt-get -y autoremove --purge patch alien
fi
chroot ubuntu apt-get clean
rm -f  ubuntu/usr/sbin/policy-rc.d
rm -rf ubuntu/tmp/* ubuntu/root/.bash_history

# Bundle & upload to S3
cp $EC2_PRIVATE_KEY ubuntu/tmp/pk.pem
cp $EC2_CERT        ubuntu/tmp/cert.pem

chroot ubuntu ec2-bundle-vol            \
  -r $bundlearch                        \
  -d /tmp                               \
  -p $prefix                            \
  -u $AWS_USER_ID                       \
  -k /tmp/pk.pem                        \
  -c /tmp/cert.pem                      \
  -s $size                              \
  -e /tmp

ec2-upload-bundle                       \
  --retry                               \
  -b $bucket                            \
  -m ubuntu/tmp/$prefix.manifest.xml    \
  -a $AWS_access_key                 \
  -s $AWS_SECRET_ACCESS_KEY

umount ubuntu/dev/pts
umount ubuntu/proc

set +x

cat <<EOF

Now you might want to run this command:
  ec2-register $bucket/$prefix.manifest.xml

EOF