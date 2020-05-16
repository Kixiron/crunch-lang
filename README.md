# Crunch

![CI](https://github.com/Kixiron/crunch-lang/workflows/CI/badge.svg)
![Security Audit](https://github.com/Kixiron/crunch-lang/workflows/Security%20Audit/badge.svg)
[![Coverage Status](https://coveralls.io/repos/github/Kixiron/crunch-lang/badge.svg?branch=master)](https://coveralls.io/github/Kixiron/crunch-lang?branch=master)
![Lines of Code](https://tokei.rs/b1/github/Kixiron/crunch-lang)
![GitHub Issues](https://img.shields.io/github/issues/Kixiron/crunch-lang)

Crunch is a strongly & statically typed systems level language focused on ease of use, portability and speed, built for the modern age.

## Building Crunch

First, have the nightly toolchains of [`rustup`] and `cargo` installed, then run the following commands

```text
git clone https://github.com/Kixiron/crunch-lang
cd crunch-lang
cargo build
```

## About

Crunch is a language that believes in strength through types. It's based on the belief that compilers, at their core, are information driven and that the more information the compiler has to reason about your program with, the more efficiently, aggressively and safely it can optimize your program.
For example, slice indexing is something often implemented as a runtime check, because it's nearly impossible to verify that indexing an arbitrary slice with an arbitrary integer will be successful, but if the slice was statically constrained to have a length of `> x && < y` and the integer used to index also has the same constraint, then indexing will always be within bounds.
Another big goal is provable infallibility, or the ability to statically prove that portions or even the entirety of your program are completely safe from irrecoverable crashes.

Crunch takes a lot of inspiration from functional languages and attempts to bring some functional concepts, for example sum types, into the realm of imperative programming.

Crunch has classes (known as types), enumerations and traits

```
:: A single user
type User
    username: String
    email: String
    privileges: Privileges
end

:: Privileges of a user
enum Privileges
    None
    Read
    Write
end

:: An admin user
type Admin
    username: String
    email: String
end

:: An account we can store
trait Account
    fn name(&self) -> &str
        empty
    end

    fn email(&self) -> &str
        empty
    end
end

:: Implement the `Account` trait on the `User` type
extend User with Account
    fn name(&self) -> &str
        &self.username
    end

    fn email(&self) -> &str
        &self.email
    end
end

:: This doesn't have to be done in actual code, but it's an example
import std.collections.Vec

fn filter_admins(users: slice[dyn Account]) -> Vec[dyn Account]
    let admin_names: arr[1, &str] := arr["Administrator"]
    let mut non_admins := Vec.new()

    for user in users
        :: A normal `if` example
        if !admin_names.contains(user.name())
            :: A `slice` isn't owned data, so we have to make a copy
            :: of it before we can take ownership of it
            non_admins.push(user.clone())
        end

        :: An example with pattern matching and match statements
        match user.name()
            :: `admin` binds the value given to the match,
            :: and the `where` clause determines if the arm matches
            admin where admin_names.contains(&admin) =>
                println("Found an admin named {}", admin)
            end

            :: `_` is used as a catch-all, and `empty` just means that nothing happens
            _ => empty
        end
    end

    :: Crunch has implicit returns, so the last 'trailing' expression
    :: will be treated as a return value
    non_admins
end

:: An alternative way to write the above function would be this
fn filter_admins(users: slice[User | Admin]) -> Vec[dyn Account]
    users.iter().filter_map(
        do |account|
            match account
                :: If the account is of type `User`, return it
                user: User =>
                    Some(user)
                end

                :: If the account is of type `Admin`, don't return it
                admin: Admin =>
                    None
                end
            end
        end
    )
    .collect()
end
```

[`rustup`]: https://rustup.rs/
