Infer
=============

A movie title reader based on the combinator parser library attoparsec.
An example of it's capabilities is shown below.

```haskell
> import Duchess.Infer
> let movie = infer "Evil Dead 2 - Dead By Dawn.mkv"
> print movie
[Title "Evil Dead 2 - Dead By Dawn"]
> title movie
Just (Title "Evil Dead 2 - Dead By Dawn")
> year movie
Nothing
> let movie2 = infer "Five.Element.Ninjas.1982.720p.BluRay.x264.FLAC-TBB.mkv"
> title movie2
Just (Title "Five Element Ninjas")
> year movie2
Just (Year 1982)
> codec movie2
Just (Codec H264)
```