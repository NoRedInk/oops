-- |
-- Description : Parsing a Markdown text into a terminal Doc format.
module Data.Text.Prettyprint.Doc.Markdown
  ( fromMarkdown,
  )
where

import "cmark" CMark
import "text" Data.Text (Text)
import qualified "text" Data.Text as T
import "prettyprinter" Data.Text.Prettyprint.Doc
  ( Doc,
    align,
    annotate,
    enclose,
    hardline,
    indent,
    parens,
    pretty,
    softline,
    vsep,
    (<+>),
  )
import "prettyprinter-ansi-terminal" Data.Text.Prettyprint.Doc.Render.Terminal
  ( AnsiStyle,
    Color (Blue, Yellow),
    bold,
    color,
    italicized,
    underlined,
  )
import qualified "prettyprinter" Data.Text.Prettyprint.Doc.Util as Util

-- | Turn a Markdown encoded text into a pretty printable Doc value.
fromMarkdown :: Text -> Doc AnsiStyle
fromMarkdown = render . untilBreak . commonmarkToNode [optNormalize, optSafe]

render :: Node -> Doc AnsiStyle
render n@(Node _ nodeType _) =
  if isBlock nodeType
    then block $ renderNoBlock n
    else renderNoBlock n

isBlock :: NodeType -> Bool
isBlock DOCUMENT = False
isBlock THEMATIC_BREAK = True
isBlock PARAGRAPH = True
isBlock BLOCK_QUOTE = True
isBlock (HTML_BLOCK _) = True
isBlock (CUSTOM_BLOCK _ _) = True
isBlock (CODE_BLOCK _ _) = True
isBlock (HEADING _) = True
isBlock (LIST _) = True
isBlock ITEM = False
isBlock (TEXT _) = False
isBlock SOFTBREAK = False
isBlock LINEBREAK = False
isBlock (HTML_INLINE _) = False
isBlock (CUSTOM_INLINE _ _) = False
isBlock (CODE _) = False
isBlock EMPH = False
isBlock STRONG = False
isBlock (LINK _ _) = False
isBlock (IMAGE _ _) = False

renderNoBlock :: Node -> Doc AnsiStyle
renderNoBlock (Node _ DOCUMENT xs) = mconcat (fmap render xs)
renderNoBlock (Node _ THEMATIC_BREAK _) = mempty
renderNoBlock (Node _ PARAGRAPH xs) = mconcat $ fmap render xs
renderNoBlock (Node _ BLOCK_QUOTE xs) =
  annotate italicized . indent 4 $ mconcat (fmap render xs)
renderNoBlock (Node _ (HTML_BLOCK text) _) = indent 4 $ pretty text
renderNoBlock (Node _ (CUSTOM_BLOCK _onEnter _onExit) xs) =
  mconcat $ fmap render xs
renderNoBlock (Node _ (CODE_BLOCK _info text) _) = indent 4 (pretty text)
renderNoBlock (Node _ (HEADING _level) xs) =
  annotate bold $ mconcat (fmap render xs)
renderNoBlock (Node _ (LIST ListAttributes {listType}) xs) =
  vsep $ uncurry (renderBullet listType) <$> zip [1 ..] xs
renderNoBlock n@(Node _ ITEM _) = renderBullet BULLET_LIST 1 n
renderNoBlock (Node _ (TEXT text) _) = reflow text
renderNoBlock (Node _ SOFTBREAK _) = softline
renderNoBlock (Node _ LINEBREAK _) = hardline <> hardline
renderNoBlock (Node _ (HTML_INLINE text) _) = enclose "`" "`" (reflow text)
renderNoBlock (Node _ (CUSTOM_INLINE _onEnter _onExit) xs) =
  mconcat $ fmap render xs
renderNoBlock (Node _ (CODE text) _) = annotate (color Yellow) $ reflow text
renderNoBlock (Node _ EMPH xs) = annotate italicized . mconcat $ fmap render xs
renderNoBlock (Node _ STRONG xs) = annotate bold . mconcat $ fmap render xs
renderNoBlock (Node _ (LINK url _) xs) = annotate (color Blue) $ linkLike url xs
renderNoBlock (Node _ (IMAGE url _) xs) = linkLike url xs

renderBullet :: ListType -> Int -> Node -> Doc AnsiStyle
renderBullet BULLET_LIST _ (Node _ ITEM xs) =
  mappend "- " . align . mconcat $ fmap renderNoBlock xs
renderBullet ORDERED_LIST i (Node _ ITEM xs) =
  mappend (pretty i <> ". ") . align . mconcat $ fmap renderNoBlock xs
renderBullet _ _ node = render node

linkLike :: Text -> [Node] -> Doc AnsiStyle
linkLike url xs@[Node _ (TEXT text) _] =
  if text == url
    then annotate underlined $ pretty url
    else fullLink url xs
linkLike url xs = fullLink url xs

fullLink :: Text -> [Node] -> Doc AnsiStyle
fullLink url xs =
  mconcat (fmap render xs) <+> parens (annotate underlined $ pretty url)

block :: Doc ann -> Doc ann
block x = hardline <> x <> hardline

-- | Like Data.Text.Prettyprint.Doc.Util.reflow,
--   only this preserves whitespace at the start or end of the text.
reflow :: Text -> Doc ann
reflow text = prefix <> Util.reflow text <> suffix
  where
    prefix =
      if T.stripStart text == text
        then mempty
        else softline
    suffix =
      if T.stripEnd text == text
        then mempty
        else softline

untilBreak :: Node -> Node
untilBreak (Node pos DOCUMENT xs) =
  Node pos DOCUMENT $ takeWhile (not . thematicBreak) xs
untilBreak n = n

thematicBreak :: Node -> Bool
thematicBreak (Node _ THEMATIC_BREAK _) = True
thematicBreak _ = False
