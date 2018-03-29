classdef poCon < oCon
%POCON   The Parallel Out-of-Core Data Container Class
%
%   x = poCon(TYPE,DIMS,ISCOMPLEX) returns a parallel out-of-core data 
%   container containing distributed data DATA.
%              
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Properties
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    properties (SetAccess = protected)
%         excoddims; % Explicit codistributed dimension of data
%         excodpart; % Explicit codistributed partition of data
%         imcoddims; % Implicit codistributed dimension of data
%         imcodpart; % Implicit codistributed partition of data
    end
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Methods
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    methods
        
        function x = poCon(type,headerIn,varargin)
            % Construct and set class attributes
            x = x@oCon(type,headerIn.dims,headerIn.complex,varargin);            
        end % Constructor
        
    end % Public methods
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Static Methods
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    methods ( Static )
        
        % delete function
        function delete(x)
            % Amazing deletion happens here            
        end % delete
        
    end % Static methods
    
end % Classdef