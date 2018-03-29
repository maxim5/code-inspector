# interface
class CommitInfo
    def initialize()
        raise LoadError, "this is a interface"
    end
    
    def new_files()
        raise NotImplementedError
    end
    
    def commit_message()
        raise NotImplementedError
    end
    
    def modified_files()
        raise NotImplementedError
    end
end
