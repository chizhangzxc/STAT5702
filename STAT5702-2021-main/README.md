# Installing Tools

Some resources to get you started.

- Install R: https://muug.ca/mirror/cran/


- Install RStudio Desktop: https://rstudio.com/products/rstudio/#rstudio-desktop

- You've already found https://github.com but you'll need an account.

- Install git on your machine
  - Windows & OS X: http://git-scm.com/downloads
  - Debian/Ubuntu: __sudo apt-get install git-core__
  
  
- Re-start RStudio and then check if RStudio can find git  by running this in the terminal tab:
    - Mac, Linux: __which git__ 
    - most versions of Windows: __where git__ 
You should get back a result like: _usr/bin/git_ or _C:\Program Files (x86)\Git_
    
Hopefully _RStudio_ can find _git_.  If not, quit and re-launch _RStudio_

For more help in getting _git_ set up for _RStudio_ see also: https://support.rstudio.com/hc/en-us/articles/200532077?version=1.2.5001

# Try RStudio with git

In https://github.com, make a new repo and click the option to initialize it with a README.md file.  Copy the repo url from the repo.  For this repo it would be: https://github.com/iamdavecampbell/STAT5702-2021.git 

_RStudio_ integrates _git_ and version control from within projects.  In _RStudio_ start a new project from *File* —> *New Project* —> *Version Control* —> *Git*.  Paste in your repo url (for this repo it would be https://github.com/iamdavecampbell/STAT5702-2021.git ).  I prefer to use the same project name as the repo name.

When you make a new project, _RStudio_ will *clone* the repo (download a local copy) and start you out in the local repo working directory.  You'll be able to add and modify repo files, then *commit* to save a snapshot of your project that you could revert to if necessary.  When you're ready to upload changes to your github repo, first *pull* to make sure that you have the most recent online version of your repo.  Then *push* to upload your changes online.  


# Feeling lucky?

- Make some changes in _RStudio_ and *commit* them, but don't *push* them yet.  
- Then make some changes to files directly from github.com and *commit* (save) them.  The online changes have been saved to the main (online) repo, but your _RStudio_ changes are modifications of an earlier version of that repo. 
- Now go back to _RStudio_ and try to *push* your latest *commit*.  You will get an error saying that you need to *pull* first.
- Now try to *pull* the latest from online and you'll need to *merge* the online and local repo versions.  It's not as messy as it sounds and it's handled pretty well via the _auto merge_.  You can also view the differences via *diff*.  
- Once the *pull* (and necessary *merge*) is complete you can *commit* and *push* your changes to the online repo.  


