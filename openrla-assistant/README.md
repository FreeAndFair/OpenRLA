# OpenRLA Assistant

## Requirements

- `stack` (1.3)
- `node` (7.6) and `npm` (4.1)

## Demoing

To setup, build, and run both the front and backend, execute `demo.sh`
in the `openrla-assistant` directory.

### Usage instructions

1. Click the "Edit Election" button
1. Enter a title and date, click "Add"
1. In the "Contest Manifest" section of the "Election", tab, click
   "Upload", and select the `TestContestManifest.json` file in the
   `backend/test/data/dominion` folder.
1. In the "Candidate Manifest" section of the "Election", tab, click
   "Upload", and select the `TestCandidateManifest.json` file in the
   `backend/test/data/dominion` folder.
1. In the "Ballot Manifest" section of the "Election", tab, click
   "Upload", and select the `TestBallotManifest.json` file in the
   `backend/test/data/dominion` folder.
1. In the "Election" tab, in the "Contest Outcomes" section, add share
   proportions for every candidate in any contest you want to audit,
   then click "Save". These share must be in the unit interval and sum
   to 1.
1. Click on the "Audit" tab, then click the "Start" button.
1. In the dialog that opens, adjust the date and risk limit as desired,
   and select at least one contest to audit. Ensure that you have
   previously entered reported vote shares for candidates in any
   contests under audit. Click the "Save" button.
1. Click the "Audit" button. In the dialog that pops up, select the
   candidate which is marked on the paper ballot. When just demoing,
   select any candidate. Click "Save".
1. You can observe the "Test Statistic" for each contest updated as each
   ballot interpretation is added.
