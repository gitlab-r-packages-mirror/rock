echo ----------
echo $(date)

### Go to directory with cloned git repo
cd ~/deploy_rock.opens.science

### Delete old 'public' directory if it exists
#rm -rf public

pwd
echo $PATH
echo Calling PkgDown

### Render the site
#/usr/local/bin/quarto render --to all
/usr/local/bin/R -e "pkgdown::build_site();"

echo Finished PkgDown

### Copy image
mkdir public/iROCK
cp -R iROCK public
mkdir public/img
cp img/hex-logo.png public/img/hex-logo.png

### Delete all contents in public HTML directory
rm -rf ~/rock.opens.science/*.*
rm -rf ~/rock.opens.science/*
rm -f ~/rock.opens.science/.htaccess

### Copy website
cp -RT public ~/rock.opens.science

### Copy .htaccess
cp -f .htaccess ~/rock.opens.science

echo ----------
