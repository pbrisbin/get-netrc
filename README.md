## Usage

```
Usage: get-netrc [-o|--output Field] [-f|--filter Field=value] [PATH]
  Read credentials out of ~/.netrc

Available options:
  -h,--help                Show this help text
  -o,--output Field        Fields to output
  -f,--filter Field=value  Filter the machines returned
  PATH                     Alternate path to ~/.netrc

Available fields: Name, Login, Password, Account
```

**NOTE**: If you pass no `-o` flags, nothing will be output.

## Output Format

If there is exactly one result and you're outputting exactly one field, the
value is printed unadorned. This is to support this tool's primary use-case:
reading passwords into other tools. Otherwise each field is shown as a single
line of the form `Field: value`

If there are more than one result, each machine will be numbered with its fields
shown indented below a heading:

```
machine N
  Field: value
  Field: value
machine N
  Field: value
  Field: value
```

**NOTE**: The numbers have no meaning, they're just indexes in the result list.

## GnuPG

`get-netrc PATH` will check for `PATH.gpg`. If found, it will invoke `gpg
--decrypt` and operate on its contents instead.

## Install

```
git clone https://github.com/pbrisbin/get-netrc
cd get-netrc
stack install
```

---

[LICENSE][./LICENSE] | [CHANGELOG][./CHANGELOG.md]
