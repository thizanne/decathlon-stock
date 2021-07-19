# Check decathlon stock

## Requirements

- Have `opam` installed and setup for easiest building
- Have a working `sendmail` conf for mail notifications

## Building

### The easiest way

Run these commands in the project root directory:

```sh
$ opam switch create . --locked # This is expected to take quite some time
$ decathlon-stock --help
```

### The other ways

Install within an already existing `opam` switch:

```sh
$ opam pin add .
$ decathlon-stock --help
```

Build and run without installing, if dependencies are met:

```sh
$ dune exec decathlon-stock -- --help
```

## Running

Get the URL of a decathlon product that you want to watch the stock
for (for instance,
`https://www.decathlon.fr/p/some-product-name/_/R-p-12345`).

Then, to check every fifteen minutes the stock of this item for the
sizes S and XL, run:

```sh
$ decathlon-stock \
    --mail your@mail.org \
    --period 15 \
    --sizes S,XL
    "https://www.decathlon.fr/p/some-product-name/_/R-p-12345"
```

The program will then run without stopping, printing the stock every
15 minutes. Upon finding some new stock for at least one of the
specified sizes, it will send a mail to the provided address. Only one
mail will be send for each size when stock becomes available. If it
then becomes out of stock, then available again, another mail will be
sent.


More doc is available under the `--help` option.
