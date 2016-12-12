{-|
Module      : WebFudgets
Description : Fudgets for Web Programming
Maintainer  : Thomas Hallgren

Web Fudgets allows you to write web application in
a functional style. It's built as a layer on top of Haste.

Fudgets is a Graphical User Interface Libary for Haskell,
created in the early 1990s. See
<http://www.altocumulus.org/Fudgets>.

Used in the course Functional Programming GU/Chalmers.
-}
module WebFudgets(-- * The Fudget type
                  F,
                  -- * Running a fudget
                  runF,
                  -- * User interface elements
                  buttonF,ButtonClick(..),
                  showF,readShowF,stringF,canvasF,
                  -- * Static content
                  textF,ahrefF,
                  -- * Layout
                  h1F,h2F,h3F,pF,tableF,divF,boxF,ulF,liF,preF,
                  -- * Fudget combinators
                  -- ** Parallel composition
                  (>+<),(>+),(+<),listF,
                  -- ** Serial composition
                  (=<=),(=>=),
                  -- ** Loops
                  loopLeftF,
                  -- * Adding application specific functionality
                  -- ** Stateless
                  mapF,filterF,mapMaybeF,idF,nullF,
                  -- ** Stateful
                  stateF
                  ) where

import Haste
import Haste.DOM
import Haste.Events
import Haste.Concurrent
import Haste.Graphics.Canvas
import Haste.Foreign(constant)
import Haste.Prim(toJSStr)

-- | The type of an event handler
type H a = a -> CIO ()

-- | @F i o@ is the type of a fudget that consumes an input stream
-- of values of type @i@ and produces an output
-- stream of values of type @o@.
-- It also generates a user interface with zero or more interface elements,
-- and can read input from and send output to those interface elements.
newtype F i o = F (H o->CIO (H i,[Elem]))

runF (F fud) = concurrent $
               do (_,es) <- fud ignore
                  viewport <- newElem "meta" `with`
                               [attr "name" =: "viewport",
                                attr "content" =: "width = device-width"]
                  appendChild documentHead viewport
                  setStyle documentBody "background" "#eee"
                  liftIO $ mapM_ (appendChild documentBody) es

instance Monoid (F i o) where
  mempty = nullF
  mappend (F f1) (F f2) = F $ \ oh -> do (_,es1) <- f1 oh
                                         (_,es2) <- f2 oh
                                         return (ignore,es1++es2)

instance Functor (F i) where
  fmap f (F fud) = F $ \ oh -> fud (oh . f)


absF f = F $ \ oh -> return (f oh,[])
nullF = absF (const ignore)
-- ^ Ignores all input. Doesn't produce any output.
idF   = absF id
-- ^ @mapF id@
mapF f = absF (. f)
-- ^ Like 'map' for lists. @mapF f@ outputs @f x@ for every @x@ in the input stream
mapMaybeF f = absF (\oh->maybe (return ()) oh . f)
-- ^ Like 'mapMaybe' for lists. A combination of mapF and filterF.
filterF p = mapMaybeF (\x->if p x then Just x else Nothing)
-- ^ Like 'filter' for lists. Propagates values from the input stream to
-- the output stream if they pass a test.
stateF f init = F $ \ oh -> do state <- newMVar init
                               let ih i = do old <- takeMVar state
                                             let (new,out) = f old i
                                             putMVar state new
                                             oh out
                               return (ih,[])

infixr 3 =<=,=>=

-- | Right-to-left serial composition. The output stream of the right fudget
-- is connected to the input stream of the left fudget.
F f1 =<= F f2 = F $ \ oh -> do (ih1,es1) <- f1 oh
                               (ih2,es2) <- f2 ih1
                               return (ih2,es1++es2)

-- | Left-to-right serial composition. The output stream of the left fudget
-- is connected to the input stream of the right fudget.
F f1 =>= F f2 = F $ \ oh -> do (ih2,es2) <- f2 oh
                               (ih1,es1) <- f1 ih2
                               return (ih1,es1++es2)

infixl 5 >+<,+<,>+
-- | Tagged parallel composition. Messages to/from the left fudget are
-- tagged 'Left'. Messages to/from the right fudget are tagged 'Right'.
F f1 >+< F f2 = F $ \ oh -> do  (ih1,es1) <- f1 (oh . Left)
                                (ih2,es2) <- f2 (oh . Right)
                                return (either ih1 ih2,es1++es2)

-- | Parallel composition where only the right fudget is connected.
-- The left fudget is typically static content.
F f1 +< F f2 = F $ \ oh -> do  (ih1,es1) <- f1 ignore
                               (ih2,es2) <- f2 oh
                               return (ih2,es1++es2)

-- | Parallel composition where only the left fudget is connected.
-- The right fudget is typically static content.
F f1 >+ F f2 = F $ \ oh -> do  (ih1,es1) <- f1 oh
                               (ih2,es2) <- f2 ignore
                               return (ih1,es1++es2)

-- | Creates a feedback loop. @loopLeftF fud@ behaves as follows:
-- output from @fud@ tagged @Left@ will be sent back to
-- the input of @fud@. Output from @fud@ tagged @Right@ will be sent to the
-- output of @loopLeftF fud@. Input to @loopLeftF fud@ will be tagged
-- @Right@ and delivered to @fud@.
loopLeftF (F fud) =
  F $ \ oh -> do loop <- newEmptyMVar
                 (ih,es) <- fud (either (putMVar loop . Left) oh)
                 forkIO $ let feed = do ih =<< takeMVar loop
                                        feed
                          in feed
                 return (putMVar loop . Right,es)


-- | Tagged parallel composition of a list of fudgets
listF tfs = F $ \ oh ->
            do (ihs,ess) <- unzip <$> sequence [f (oh . (,) t) | (t,F f)<-tfs]
               let tihs = zip (map fst tfs) ihs
                   ih (t,i) = maybe (return ()) ($ i) (lookup t tihs)
               return (ih,concat ess)

--------------------------------------------------------------------------------
divF = wrapF "div"
-- ^ A div element @\<div>...\</div>@
pF = wrapF "p"
-- ^ Paragraph @\<p>...\</p>@
h1F = wrapF "h1"
-- ^ Level 1 header @\<h1>...\</h1>@
h2F = wrapF "h2"
-- ^ Level 2 header @\<h2>...\</h2>@
h3F = wrapF "h2"
-- ^ Level 3 header @\<h3>...\</h3>@

ulF = wrapF "ul"
-- ^ Unordered list @\<ul>...\</ul>@

liF = wrapF "li"
-- ^ List item @\<li>...\</li>@

-- | A div element with a black border and some padding
boxF = wrapF' "div" [style "border" =: "1px solid black",
                     style "padding" =: "1ex",
                     style "margin" =: "1ex"]

preF = wrapF "pre"
-- ^ A pre element @\<pre>...\</pre>@

wrapF = layoutF . wrap

wrapF' tagname as = layoutF (wrap' tagname as)

layoutF layout (F fud) =
  F $ \ oh -> do (ih,es) <- fud oh
                 e <- layout es
                 return (ih,[e])

-- | A table with @n@ columns
tableF = layoutF . tableL

tableL n es = do tds <- mapM (wrap1 "td") es
                 rows <- mapM (wrap "tr") (chop n tds)
                 wrap "table" rows

-- | Plain text
textF s = F $ \ oh ->
          do el <- newTextElem s
             return (ignore,[el])

-- | Hyperlink @\<a href="url">...\</a>@
ahrefF url = layoutF (wrap' "a" [attr "href" =: url])

--------------------------------------------------------------------------------

showF :: Show i => F i o
showF = nullF =<= inputF' [prop "disabled" =: "true"] =<= mapF show

-- | 'stringF' combined with 'show' and 'read'
readShowF :: (Show a,Read a) => F a a
readShowF = mapMaybeF readM =<= stringF =<= mapF show

readM x = case reads x of
            [(x,s)] | lex s == [("","")] -> Just x
            _ -> Nothing


-- | A string input/output field, @\<input type="text">@
stringF = inputF "text"

inputF ty = inputF'  [prop "type"=:ty]

inputF' ps =
  F $ \ oh ->
  do inp <- newElem "input" `with` ps
     onEvent inp Change $ \_ -> do oh =<< getProp inp "value"
     return (setProp inp "value",[inp])


data ButtonClick = BtnClick deriving (Eq,Show,Read)

buttonF lbl =
  F $ \ oh ->
  do btn <- newElem "input"`with` [prop "type"=:"button"]
     setProp btn "value" lbl
     onEvent btn Click $ \_ -> oh BtnClick
     return (oh,[btn])

-- | Creates a canvas of given width and height. Use the Picture type
-- from "Haste.Graphics.Canvas" to draw things.
canvasF :: (Int,Int) -> F (Picture ()) o
canvasF (width,height) =
  F $ \ oh ->
  do el <- newElem "canvas" `with` [style "border" =: "1px solid black",
                                    style "background" =: "white",
                                    style "width" =: (show width++"px"),
                                    style "height" =: (show height++"px"),
                                    prop "width" =: show (k*width),
                                    prop "height" =: show (k*height)]
     Just canvas <- getCanvas el
     return (render canvas . scale (k,k),[el])
  where
    k :: Num a => a
    k = fromIntegral devicePixelRatio

--------------------------------------------------------------------------------


ignore _ = return ()

chop n [] = []
chop n xs = xs1:chop n xs2
     where (xs1,xs2) = splitAt n xs

wrap1 tagname = wrap tagname . (:[])

wrap tagname = wrap' tagname []

wrap' tagname as es = do e <- newElem tagname `with` as
                         mapM_ (appendChild e) es
                         return e


devicePixelRatio = constant (toJSStr "window.devicePixelRatio") :: Int

documentHead = constant (toJSStr "document.head") :: Elem
