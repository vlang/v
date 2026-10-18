# Cleanup around Result and Option propagation

With `-autofree` or `-ownership`, a failed `!` or `?` propagation from a Result or Option
function runs its deferred code before returning. Nested blocks in deferred code keep their
own cleanup scopes. Later locals are destroyed only inside their declaring scopes, after
initialization; a failed propagation does not destroy locals that were never initialized.

Successful propagation continues with the usual lexical cleanup order. Under `-ownership`,
moving a value to another function transfers responsibility for its destruction.
