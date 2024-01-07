TODO update

# tabbycat-feedback

After you've run a debating tournament with Tabbycat, you can use this program
to generate anonymised feedback sheets for your adjudicators. Each adjudicator
receives one sheet containing the scores and comments from each round. The
names of teams and adjudicators providing feedback (as well as the teams'
positions in the debate) are not included in the sheets, but note that people
can often guess who the feedback comes from anyway. In particular, when wings
receive feedback from their chair for a specific round, this is hardly
anonymous.

## Installation

I don't provide a ready-made executable version of this program, so you'll have
to build it yourself:

- Install [ghcup](https://haskell.org/ghcup).
- Use `ghcup` to install `cabal` (any version will do) and GHC 9.4.4.
- Run `cabal build` in this directory.

## Usage

You can now run `tabbycat-feedback` with this command:

```text
cabal run -- tabbycat-feedback --token <TOKEN> --url <URL> --basedir output
```

where

- `<TOKEN>` is your Tabbycat API token.
- `<URL>` is the base API URL of your tournament. The URL looks like this:
  `https://<host>/api/v1/tournaments/<tournament>`, where
  `<host>` is your Tabbycat server and `<tournament>` is your tournament slug.

The command will (after some time) generate some files in the `output`
directory:

- One file `<key>.html` for each adjudicator, where `<key>` is that
  adjudicator's URL key.
- One file `style.css`, which is a stylesheet needed to display the HTML files
  correctly.

You can now upload these files to a server, then point each adjudicator to
the URL where they can find their HTML file.

## Cleanup

After you're done with `tabbycat-feedback`, you can:

- Remove `~/.ghcup` and `~/.cabal`. This removes Cabal, GHC and the compiled
  dependencies `tabbycat-feedback`.
- Remove the folder `dist-newstyle` in this directory, which was created by
  `cabal` when building `tabbycat-feedback`.
- Remove this directory entirely.
