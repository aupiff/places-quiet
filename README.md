# [Places quiet](http://placesquiet.com)

A list of public places that don't play music. If you know of quiet eateries or
shops, please open a pull request or email blankman@boustro.com.

Quiet place data in [data.yaml](data.yaml).

## Running the mockup server

```
$ cd mockups
$ ./build v6
```

## Publishing the site to S3

```
cd publish
stack install
cd ..
publish mockups/v6
```
