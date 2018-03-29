#
# interface between cluster and its component instances, volumes, snapshots,
# security_groups and keypairs
#
module Wucluster
  class Cluster

    #
    # Bookkeeping of instances and volumes
    #

    # Hash of volumes, indexed by [role, instance_idx, instance_vol_idx]
    # Ex:
    #    p cluster.volumes[['master', 0, 0]]
    #    => #<struct Wucluster::Volume cluster="gibbon", role="master", instance=<Wucluster::Instance ...>, volume_idx=0, device="/dev/sdh", volume_point="/mnt/home", volume=#<Wucluster::Volume ...> >
    def all_volumes
      load! if @all_volumes.nil?
      @all_volumes
    end
    # flat list of volumes
    def volumes
      all_volumes.sort_by(&:first).map(&:last)
    end
    # Volume with the given role and index
    def find_volume cluster_vol_id
      all_volumes[ cluster_vol_id ]
    end

    # Hash of instances, indexed by [role, instance_idx]
    # Ex:
    #    p cluster.instances[['master', 0, 0]]
    #    => #<struct Wucluster::Instance cluster="gibbon", role="master", idx=0, instance=...>
    def all_instances
      load! if @all_instances.nil?
      @all_instances
    end
    # flat list of instances
    def instances
      all_instances.sort_by(&:first).map(&:last)
    end
    # Instance with the given role and index
    def find_instance cluster_node_id
      all_instances[ cluster_node_id ]
    end

    # enumerate all roles found among this cluster's instances
    def roles
      roles_count.keys.uniq
    end
    # map of roles to count of instances with that role
    def roles_count
      roles_count = Hash.new{|h,k| 0 }
      all_instances.values.each do |instance|
        roles_count[instance.role] += 1
      end
      roles_count
    end

    # flat list of snapshots from all volumes
    def snapshots
      Volume::Snapshot.for_cluster(self)
    end

  end
end
