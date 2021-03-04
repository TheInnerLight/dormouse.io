---
title: "Dormouse Client - Testing"
author: Me
date: Feb 8, 2021
tags: [haskell, dormouse]
description: Dormouse-Client-Testing
isTop: False
---

## Testing

Dormouse-Client has been designed to facilitate easy testing.  The expectation is that, most of the time, you will want to test code based on Dormouse client by a type class instance where you supply an implementation in one of two forms.

```haskell
expectBs :: HttpRequest scheme method S.ByteString contentTag acceptTag -> m (HttpResponse S.ByteString)
```

or

```haskell
expectLbs :: HttpRequest scheme method L.ByteString contentTag acceptTag -> m (HttpResponse L.ByteString)
```

These alternatives are HTTP requests and responses where the content bodies are simply Strict or Lazy `ByteString`s.  Eliminating the polymorphic content body type in favour of `ByteString` is incredibly useful for testing because it allows the same function to generate responses which map to multiple different Haskell types.

## Test Module

Note that Dormouse-Client is not fundamentally implemented with a type class containing functions of this form, this is because Dormoues-Client uses Streamly to handle the transmission of content bodies.  Dealing with streams directly is fine, if truly neccessary, but it also opens up a number of possibilities for error that we'd generally like to avoid in a high level library such as this one (e.g. a user accidentally attempting to consume a request body multiple times.)

To get around this, we provide  a special type class to facilitate testing in this form.  To access this, import the Test Class module:

```haskell
import Dormouse.Client.Test.Class
```

within which you will find a type class with these properties:

```haskell
class Monad m => MonadDormouseTestClient m where
  expectLbs :: IsUrl url 
            => HttpRequest scheme method L.ByteString contentTag acceptTag -> m (HttpResponse L.ByteString)
  expectBs :: IsUrl url 
           => HttpRequest scheme method S.ByteString contentTag acceptTag -> m (HttpResponse S.ByteString)
  {-# MINIMAL expectLbs | expectBs #-}
```

**Note:** This class has an Orphan instance of `MonadDormouseClient` so you should be careful this module and keep it tightly confined to test cases.

Assuming that you have some Test monad `TestM`, which is configured something like this:

```haskell
newtype TestM a = TestM
  { unTestM :: ReaderT TestEnv IO a 
  } deriving (Functor, Applicative, Monad, MonadReader TestEnv, MonadIO, MonadThrow)
```

You can then go ahead and create an instance of `MonadDormouseClient`.

```haskell
instance MonadDormouseTestClient TestM where
  expectBs (r @ HttpRequest { requestUrl = url, requestMethod = method, requestBody = body, requestHeaders = headers }) = 
    let reqUrl = asAnyUrl url in
    case (reqUrl, method) of
      ([url|https://starfleet.com/captains|], GET) -> do
        resp <- ... -- implementation here!
        pure $ HttpResponse
          { responseStatusCode = 200
          , responseHeaders = [("Content-Type", "application/json")]
          , responseBody = resp
          }
```

This provides a re-usable template that we can use to implement tests and ensure that our requests are being constructed correctly and that responses of the expected form can be handled.

**Note:** 
- Using the line `let reqUrl = asAnyUrl url` before pattern matching on `url` is crucial because it allows the funtion to work polymorphically across different `Url` types, e.g. `Http`, `Https` or `AnyUrl`.
- Pattern matching a literal URL with the `url` QuasiQuoter requires the `ViewPatterns` language extension.





