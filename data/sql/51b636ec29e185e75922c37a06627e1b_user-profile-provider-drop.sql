--
-- Implementation of the profile provider interface for users.
--
-- @author <a href="mailto:yon@openforce.net">yon@openforce.net</a>
-- @version $Id: user-profile-provider-drop.sql,v 1.2 2002/07/08 22:10:21 chak Exp $
--

create function inline_0()
returns integer as '
begin

    -- drop the binding between this implementation and the interface it
    -- implements.
    perform acs_sc_binding__delete (
        ''profile_provider'',
        ''user_profile_provider''
    );

    -- drop the bindings to the method implementations

    -- name method
    perform acs_sc_impl_alias__delete (
        ''profile_provider'',
        ''user_profile_provider'',
        ''name''
    );

    -- prettyName method
    perform acs_sc_impl_alias__delete (
        ''profile_provider'',
        ''user_profile_provider'',
        ''prettyName''
    );

    -- render method
    perform acs_sc_impl_alias__delete (
        ''profile_provider'',
        ''user_profile_provider'',
        ''render''
    );

    -- drop the implementation
    perform acs_sc_impl__delete(
        ''profile_provider'',
        ''user_profile_provider''
    );

    return 0;

end;' language 'plpgsql';

select inline_0();
drop function inline_0();


