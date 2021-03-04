---
title: "Dormouse Client"
author: Me
date: Jan 31, 2021
tags: [haskell, dormouse]
description: Dormouse-Client
isTop: False
---

# Dormouse-Client

Dormouse client is an HTTP client that will help you REST.

It was designed with the following objectives in mind:

- HTTP requests and responses should be modelled by a simple, immutable Haskell Record.
- Actual HTTP calls should be made via an abstraction layer (MonadDormouseClient) so testing and mocking is painless.
- Illegal requests should be unrepresentable, such as HTTP GET requests with a content body.
- It should be possible to enforce a protocol (e.g. https) at the type level.
- It should be possible to handle large request and response bodies via constant memory streaming.

## Download

[![Dormouse-Client on Hackage](https://hackage.haskell.org/static/icons/ic_haskell_grayscale_32.svg) Dormouse-Client on Hackage](https://hackage.haskell.org/package/dormouse-client)

## Quick Start 

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.IO.Class
import Data.Aeson.TH 
import Dormouse.Client
import GHC.Generics (Generic)
import Dormouse.Url.QQ

data UserDetails = UserDetails 
  { name :: String
  , nickname :: String
  , email :: String
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions ''UserDetails

data EchoedJson a = EchoedJson 
  { echoedjson :: a
  } deriving (Eq, Show, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 6} ''EchoedJson

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  runDormouseClient (DormouseClientConfig { clientManager = manager }) $ do
    let 
      userDetails = UserDetails 
        { name = "James T. Kirk"
        , nickname = "Jim"
        , email = "james.t.kirk@starfleet.com"
        }
      req = accept json $ supplyBody json userDetails $ post [https|https://postman-echo.com/post|]
    response :: HttpResponse (EchoedJson UserDetails) <- expect req
    liftIO $ print response
    return ()
```

## GHC Extensions

The following GHC extensions are recommended:

  - OverloadedStrings
  - MultiParamTypeClasses
  - ScopedTypeVariables
  - FlexibleContexts
  - DataKinds
  - TemplateHaskell
  - QuasiQuotes

## Future Development

- Multipart HTTP requests
- Stream request/responses directly to/from files
- TBD 

## Motivation

There are many great libraries in Haskell already for creating HTTP clients already so before beginning a project like this, it is always wise to question whether the work is truly *necessary*.

First of all, it is my belief that the simplest conception of HTTP requests in Haskell can be approximated by something of the form `Request a -> IO (Response b)`.  Dormouse deviates from that form only so far as is neccessary to provide additional expressiveness and safety against constructing incorrect requests.  Requests and Responses are simple, immutable Haskell records that you can incrementally build and manipulate to your hearts content, usually using handy convenience functions to trasform them.

`http-client` is a great library upon which almost all the other http libraries (including Dormouse) are based and exposes an API pretty close to this form.  It is very powerful and supports many use cases but is very intentionally built as a base layer upon which to build more abstract libraries that help you avoid details like worrying about encoding in `ByteString`-based Urls, using popular serialisation formats like `JSON` or providing higher level streaming interfaces.

Then there are other high level libraries such as `Wreq` and `Req` which offer a vastly more type-safe experience but, for various reasons, deviate from this incremental building of HTTP requests that users of e.g. [http4s](https://http4s.org/) in the Scala community might be familiar with.

`Req` is the most type-safe of the two, indeed it makes extensive use of the type system to protect against many of the same classes of errors as Dormouse, avoiding e.g. `String`s for URLs.  Unfortunately, it posed some other difficulties for my use cases, the requirement to provide the precise data type of the response in the request makes it impossible to abstract away the call into something that can be unit tested if you have HTTP Responses that return more than one type of data.

Not having a model type to represent the HTTP Request also proved particularly challenging for me.  I like to be able to separate out whatever code builds HTTP requests from my core logic so that I can unit test and make sure everything is being reliably assembled.  That means I needed a model that allowed me to see the content body, request headers, url and request method all fully assembled and ready to send in one place so that I can categorically determine whether the assembled request is correct or erroneous.

Perhaps most egregiously, `Req` seperates out some components which are logically part of the Url such as Port and Query Parameters as well as HTTP Headers into an `Options` data type which serves as a kind of bucket for things that have no other logical place to live.  This kind of behaviour could be pretty confusing if you alter a config file to change the port you are connecting to without realising that change will not be respected by the library.

I should be clear at this point that the purpose of this section is not to criticise those other libraries or argue that Dormouse is better, merely to highlight different design decisions that have been made between them and Dormouse.  If you are already perfectly happy and feel productive with those libraries then there is absolutely no reason to switch.  If, like me, you find these problems frustrating, Dormouse offers an alternative point on the design space that might be worth a try!
