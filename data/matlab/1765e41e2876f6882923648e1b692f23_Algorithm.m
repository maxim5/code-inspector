classdef Algorithm < BatHandle
    
    properties
%         name = 'algorithm'
    end
    
    methods
        function [ this ] = Algorithm(  )
            if nargin == 0
                this.setName('alg');
                return
            end
        end
    end
    
    methods ( Static = true )
    end
    
end
