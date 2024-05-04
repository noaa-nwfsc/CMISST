# Instructions for including your repository in the noaa-nwfsc organization

## Collaborators

If you have NMFS collaborators (i.e. have noaa.gov email) on your repository who are not members of the NMFS GitHub Enterprise Cloud account, have them complete a [user request](https://sites.google.com/noaa.gov/nmfs-st-github-governance-team/github-users). If they are not NOAA FTEs or Affiliates, contact an administrator to help you add them to your repository or to transfer in your repository with outside collaborators.  You will find the list of admins in the [noaa-nwfsc Google folder](https://drive.google.com/drive/folders/1k54HDpe6AcpfZ9LZzdIARFbH6wdi8pGl?usp=sharing).

## For repositories migrated to noaa-nwfsc organization

1) Update your `README.md` file to include the disclaimer and an open access license. See below for a description of licenses.
2) Add a description and info on who created the content (otherwise the org managers will not know who to contact).
3) Add tags (far right side on repo) to help users find repositories. See the other repositories for examples.
4) Add an open LICENSE file. For government work, we are required to use an open LICENSE. If non-government FTEs were contributors and the repository does not yet have an open license on it, make sure all parties agree before applying an open source license. For data/documentation use CC0 and for code use preferrably Apache 2.0. Both are public domain type licenses.
5) Add the file `.github/workflows/secretSCAN.yml`. This will check for token and keys that are accidentally committed to a repository.

## For new repositories in noaa-nwfsc organization

1) Add a description and info on who created the content to this `README.md` file (otherwise the org managers will not know who to contact).
2) Add tags (far right side on repo) to help users find repositories. See the other repositories for examples.
3) Confirm that the License (see License tab) is appropriate for the content in your repository. Adding an open LICENSE file at the start of work clarifies that this work is open source (public domain) to any future contributors to the work. 
4) Confirm that the file `.github/workflows/secretSCAN.yml` is in this repository. This will check for token and keys that are accidentally committed to a repository.
5) Once you have completed the first four steps, delete the instructions from this `README.md`.

# Disclaimer

This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project content is provided on an "as is" basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.

