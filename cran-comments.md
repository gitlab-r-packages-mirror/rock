Hey there,

In response to Benjamin Altmann's requests, I've made the following changes in this version:

- Added a URL to a related resource (the open access ROCK book) to the Description in the DESCRIPTION.

- Added \value specifications to the listed manual pages through roxygen2's @return directive

In addition, I made these two changes:

- I updated the associated website to https://rock.opens.science
- I updated the maintainer address to rock@opens.science, which forwards to not just me, but also to Szilvia Zorgo, so we're less likely to miss messages about, to name a wild example, impending CRAN archival :-)

As always, thank you for your work on CRAN!!!

Kind regards,

Gjalt-Jorn

---

Submission on 2022-12-12:

Sorry for the successive uploads! Here's an overview of what each entails. It's just super-weird, an example that only takes < 1 second for me, takes >15 seconds on the server for some reason...

0.6.5: Disabled one command in an example, takes too long to test.

0.6.4: Shortened time for one example even further.

0.6.3: Edited examples to run faster by selecting a sub-set of sources.

0.6.2: Caved and removed {textreader} from suggests, instead porting functions to {rock}. Also added rock::get_utterances_and_codes_from_source().

0.6.1: Implemented a check on an example for an optional (non-CRAN) package.

0.6.0: Removed the 'hard' dependency on {textreadr} as it's been archived (but hopefully will return). Also added functionality to synchronize multiple data streams.

