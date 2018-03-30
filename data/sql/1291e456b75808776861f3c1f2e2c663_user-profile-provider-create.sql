--
-- Implementation of the profile provider interface for users.
--
-- @author <a href="mailto:yon@openforce.net">yon@openforce.net</a>
-- @version $Id: user-profile-provider-create.sql,v 1.1 2002/03/29 16:56:10 ben Exp $
--

    -- create the implementation
    select acs_sc_impl__new(
        'profile_provider',
        'user_profile_provider',
        'user_profile_provider'
    );

    -- add the bindings to the method implementations

    -- name method
    select acs_sc_impl_alias__new (
        'profile_provider',
        'user_profile_provider',
        'name',
        'user_profile_provider::name',
        'TCL'
    );

    -- prettyName method
    select acs_sc_impl_alias__new (
        'profile_provider',
        'user_profile_provider',
        'prettyName',
        'user_profile_provider::prettyName',
        'TCL'
    );

    -- render method
    select acs_sc_impl_alias__new (
        'profile_provider',
        'user_profile_provider',
        'render',
        'user_profile_provider::render',
        'TCL'
    );

    -- bind this implementation to the interface it implements
    select acs_sc_binding__new(
        'profile_provider',
        'user_profile_provider'
    );
