# Troubleshooting
#
# Long "Waiting for VM to boot"?
# -> forcefully power off the VM in the VirtualBox GUI and run "vagrant up" again.
#
Vagrant.configure("2") do |config|

  config.vm.box = "raring32"
  config.vm.box_url = "http://cloud-images.ubuntu.com/vagrant/raring/current/raring-server-cloudimg-i386-vagrant-disk1.box"

  config.vm.provision :shell, :inline => "
    echo 'golang-go golang-go/dashboard boolean true' > preseed.conf
    sudo debconf-set-selections preseed.conf 
  "

  config.vm.provision :shell, :inline => "
    sudo apt-get install -y algol68g bash beef boo clisp clojure1.4 \
    coffeescript f2c fp-compiler g++ gauche gawk gcc gforth gfortran ghc \
    gnat gnu-smalltalk gobjc golang groovy icont intercal iverilog \
    jasmin-sable llvm lua5.2 make mono-devel mono-mcs nodejs ocaml octave \
    open-cobol openjdk-6-jdk parrot perl php5-cli pike7.8 python r-base \
    regina-rexx ruby1.9.3 scala swi-prolog tcc tcl8.5 ucblogo valac
  "

end
