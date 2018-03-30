# Very basic flavour
# $flavour = 'tomato'
# Class: sauce::tomato (flavour: 'tomato')
#
# This class configures the dev servers ON TOP of sauce
#
# Parameters:
#   machine_description: something that describes the machine itself
#
#   cluster_description: something that describes the cluster,
#      possibly given verbosely by the site.pp thingy :)
#
# Actions:
#   ensure that the machine has an agreed directory structure.
#   Creates dirs/files as per wiki docs (/opt/riccardo/, ...)
#
# Sample Usage:
#   class { 'sauce::tomato':
#     machine_description => 'mandatory description for machine',
#     cluster_description => 'mandatory description from node.pp',
#   }
class sauce::tomato ($machine_description, $cluster_description) {
  include sauce

  $tomato_debian_packages = [
    'python-pip',                  # equivalent of gems for python
  ]

  $python_pips = [
    'blueprint',          # amazing blueprint package
  ]

  #TODO pip install $python_pips

  file { "$sauce::basepath/MACHINE_DESCRIPTION.tomato":
    ensure  => present,
    content => "$machine_description\n",
    require => File[$sauce::basepath],
  }
  file { "$sauce::basepath/CLUSTER_DESCRIPTION.tomato":
    ensure  => present,
    content => "$cluster_description\n",
    require => File[$sauce::basepath],
  }

}