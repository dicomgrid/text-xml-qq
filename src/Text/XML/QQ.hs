{-# LANGUAGE TemplateHaskell, QuasiQuotes, UndecidableInstances #-}


-- | The XML quasiquoter.
--
--    Given the variables
--
--      >  url = "google.se"
--      >  elem = "gmail"
--      >  attrNs = "something"
--      >  attrName = "Pelle"
--      >  attrValue = "Arne"
--      >  elemCont = CRef "testing"
--      >  cont1 = Elem $ element { elName = qname "hej" }
--      >  cont2 = CRef "other test"
--
--    the code
--
--      >   [$xmlQQ|
--      >   <{url}:{elem} {attrNs}:{attrName}={attrValue} attr="cool">
--      >     <elem ns1:elem1="1" ns2:elem2="2"><<elemCont>></elem>
--      >     <elem />
--      >     <el />
--      >     <<cont1>>
--      >     <<cont2>>
--      >   </{url}:{elem}>
--      >   |]
--
--    will generate the data structure
--
--      >   element {
--      >     elName = QName elem Nothing (Just url),
--      >     elAttribs = [Attr (QName attrName Nothing (Just attrNs)) attrValue,
--      >                  Attr (qname "attr") "cool"],
--      >     elContent = [
--      >       (Elem $ element { elName = qname "elem",
--      >                         elAttribs = [Attr (QName "elem1" Nothing (Just "ns1")) "1",
--      >                                      Attr (QName "elem2" Nothing (Just "ns2")) "2"],
--      >                         elContent = [elemCont]
--      >                        }),
--      >        (Elem $ element { elName = qname "elem" }),
--      >        (Elem $ element { elName = qname "el" }),
--      >        cont1,
--      >        cont2]
--      >   }
module Text.XML.QQ (xmlQQ, xmlFileQQ) where

-- import Text.XML.Light
import qualified Text.XML.Light.Types as XT
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Maybe
import Data.Monoid (Endo(..))
import Data.Foldable (foldMap)

-- import Data.Ratio
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error


xmlQQ :: QuasiQuoter
xmlQQ = QuasiQuoter xmlExp xmlPat xmlType xmlDec
  where
  xmlPat = undefined
  xmlType = undefined
  xmlDec = undefined

  xmlExp :: String -> Q Exp
  xmlExp txt =
    case parsed' of
      Left err -> error $ "Error in jsonExp: " ++ show err
      Right val -> elementToExp val
    where
      parsed' = parse xmlElementParser "txt" txt

xmlFileQQ :: QuasiQuoter
xmlFileQQ = quoteFile xmlQQ

-- Data types to Exp

elementToExp :: ElementMeta -> Q Exp
elementToExp (Element name attribs contents line) =
  [| XT.Element
       $(qnameToExp name)
       $(listE $ map attrToExp attribs)
       $(appEndo (foldMap (Endo . contentToExp) contents) [| [] |])
       Nothing
   |]

qnameToExp :: QNameMeta -> Q Exp
qnameToExp (QName name uri prefix) =
  [| XT.QName
       $(stringmetaToExp name)
       Nothing
       $(maybe [| Nothing |]
               (\p -> [| Just $(stringmetaToExp p) |])
               prefix)
   |]

stringmetaToExp :: StringMeta -> Q Exp
stringmetaToExp (StringMetaNormal s) = stringE s
stringmetaToExp (StringMetaVar s) = varE $ mkName s

attrToExp :: AttrMeta -> Q Exp
attrToExp (Attr name val) =
  [| XT.Attr
       $(qnameToExp name)
       $(stringmetaToExp val)
   |]

contentToExp :: ContentMeta -> Q Exp -> Q Exp
contentToExp (Elem e) qe =
  [| XT.Elem $(elementToExp e) : $(qe) |]
contentToExp (CRef s) qe =
  [| XT.CRef $(stringE s) : $(qe) |]
contentToExp (ContentVar v) qe =
  [| $(varE $ mkName v) : $(qe) |]
contentToExp (ContentListVar v) qe =
  [| $(varE $ mkName v) ++ $(qe) |]
contentToExp _ _ = error "Case Text in contentToExp is not implemented yet."

blank_meta_element :: ElementMeta
blank_meta_element = Element (QName (StringMetaNormal "") Nothing Nothing) [] [] Nothing

-- Data types

data AttrMeta =
  Attr {
    attrKey :: QNameMeta,
    attrVal :: StringMeta
  }

data ElementMeta =
  Element {
    elName :: QNameMeta,
    elAttribs :: [AttrMeta],
    elContent :: [ContentMeta],
    elLine :: Maybe Line
  }

data QNameMeta =
  QName	{
    qName :: StringMeta,
    qURI :: Maybe String,
    qPrefix :: Maybe StringMeta
  }

data StringMeta =
  StringMetaNormal String
  | StringMetaVar String

getStringMeta :: StringMeta -> String
getStringMeta (StringMetaNormal n) = n
getStringMeta (StringMetaVar n) = "{" ++ n ++ "}"

data ContentMeta =
  Elem ElementMeta
  | Text CDataMeta
  | CRef String
  | ContentVar String
  | ContentListVar String

data CDataMeta =
  CData	{
    cdVerbatim :: XT.CDataKind,
    cdData :: String,
    cdLine :: Maybe Line
  }


-- Parser

xmlElementParser :: Parser ElementMeta
xmlElementParser = do
  spaces
  char '<'
  name <- nameParser
  spaces
  attrs <- many $ try attrParser
  spaces
  -- string "/>"
  contents <- closeTag <|> (openCloseTag name)
  spaces
  return $ Element name attrs contents Nothing

closeTag :: Parser [ContentMeta]
closeTag = do
  string "/>"
  return []

openCloseTag :: QNameMeta -> Parser [ContentMeta]
openCloseTag (QName name Nothing ns) = do
  -- string ">"
  contents <- between (string ">") (string "</") (many contentParser)
  string $ (ns' ++ name') ++ ">"
  return contents
  where
    name' = getStringMeta name
    ns' = maybe "" (\n -> (getStringMeta n) ++ ":") ns

attrParser :: Parser AttrMeta
attrParser = do
  spaces
  name <- nameParser
  char '='
  value <- metaStringParser --between (string "\"") (string "\"") (chars)
  return $ Attr name value

contentParser :: Parser ContentMeta
contentParser = do
  spaces
  content <- (try contentVarParser) <|>
             (try xmlElementParser >>= return . Elem) <|>
             (crefParser >>= return . CRef)
  spaces
  return content

contentVarParser :: Parser ContentMeta
contentVarParser = do
  string "<<"
  ctor <- (char '*' >> return ContentListVar) <|> return ContentVar
  s <- symbol
  string ">>"
  return $ ctor s

crefParser :: Parser String
crefParser = many1 (noneOf "><")

nameParser :: Parser QNameMeta -- (String,Maybe String)
nameParser = do
  name1 <- metaSymbolParser -- symbol
  name2 <- optionMaybe (
    do
      char ':'
      metaSymbolParser)
      -- symbol)
  let
    ns = maybe Nothing (\n -> Just ( name1)) name2
    name = maybe name1 (\n -> n) name2
  return $ QName ( name) Nothing ns -- (name, ns)


-- helpers

metaStringParser :: Parser StringMeta
metaStringParser = do
  metaNormalStringParser <|> metaVarSymbolParser

metaNormalStringParser :: Parser StringMeta
metaNormalStringParser = do
  char '"'
  s <- chars
  char '"'
  return $ StringMetaNormal s

metaSymbolParser :: Parser StringMeta
metaSymbolParser = do
  metaNormalSymbolParser <|> metaVarSymbolParser

metaNormalSymbolParser :: Parser StringMeta
metaNormalSymbolParser = do
  s <- symbol
  return $ StringMetaNormal s

metaVarSymbolParser :: Parser StringMeta
metaVarSymbolParser = do
  char '{'
  s <- symbol
  char '}'
  return $ StringMetaVar s

symbol :: CharParser () String
symbol = many1 (noneOf "{}\\ \"/:;><$=")

chars :: CharParser () String
chars = many (noneOf "\"")


