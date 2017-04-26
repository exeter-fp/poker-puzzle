Edited from IRC (http://irclogger.com/.haskell-beginners/2017-04-25):

```
<nickager> is there a neater way of writing: if isRoyalFlush then Just $ RoyalFlush cards else Nothing 
<nickager> eg Bool -> Maybe 
<merijn> guard? 
<Iceland_jack> guard or monad comprehension 
<merijn> :t \b -> () <$ guard b 
<lambdabot> Alternative f => Bool -> f () 
<Iceland_jack> do guard isRoyalFlush; pure (RoyalFlush cards) 
<merijn> > (\b -> () <$ guard b) True :: Maybe () 
<lambdabot> Just () 
<merijn> > (\b -> () <$ guard b) False :: Maybe () 
<lambdabot> Nothing 
<merijn> Replace () with whatever you want 
<nickager> that looks more complex than: “if Bool then Just x else Nothing” 
<merijn> :t (<$) 
<lambdabot> Functor f => a -> f b -> f a 
<merijn> <$ is basically just "replace a value inside a Functor" 
<merijn> :t guard 
<lambdabot> Alternative f => Bool -> f () 
<Iceland_jack> > 'a' <$ [1..10] 
<lambdabot> "aaaaaaaaaa" 
<merijn> And guard produces either 'empty :: Alternative f => f a' (which is specific to 'f' but usually conforms to 'failure', like Nothing. Or it produces 'f ()') 
<nickager> Ok, I was with you until Iceland_jack pulled 'a' <$ [1..10] 
<merijn> nickager: Well, [1..10] is [Int]? yes 
<merijn> :t (<$) 
<lambdabot> Functor f => a -> f b -> f a 
<nickager> yes 
<merijn> <$ takes an 'a' and an 'f b' and turns it into 'f a', and there's really only one way to do that 
<nickager> OK 
<merijn> Replace all b's with a's, i.e. replace all Int with Char in his example 
<merijn> Since <$ only has one Char (the one you give it) it replaces all Int with that one Char 
<merijn> nickager: It's basically just "fmap (const 'a')" 
<merijn> Where "const a b = b" 
<nickager> OK 
<merijn> For lists it's generally not very useful, but <$ is nice for things like guard and parsers 
<Iceland_jack> :t (<$) :: Char -> [Int] -> String 
<lambdabot> Char -> [Int] -> String 
<merijn> Where you want to use something for it's side-effects (parsing, failing, etc.) but want to replace the "success" result 
<merijn> > True <$ Just 'c' 
<lambdabot> Just True 
<merijn> > True <$ Nothing 
<lambdabot> Nothing 
<nickager> So: “RoyalFlush cards <$ guard isRoyalFlush” is the applicative form of do guard isRoyalFlush; pure $ RoyalFlush cards 
<merijn> nickager: Right 
<merijn> nickager: I just dislike the "do { ...; pure something }" in do notation :) 
<Iceland_jack> Or MonadComprehensions [ RoyalFlush cards | isRoyalFlush ] 
<Iceland_jack> does the same 
<Iceland_jack> not supported by lambdabot tho 
<nickager> OK  
<nickager> thanks both 
```