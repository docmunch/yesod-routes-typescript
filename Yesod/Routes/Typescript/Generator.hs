module Yesod.Routes.Typescript.Generator (genTypeScriptRoutes) where

import ClassyPrelude
import Data.Text (dropWhileEnd)
import qualified Data.Text as DT
import Filesystem (createTree)
import Data.Char (isUpper)
import Yesod.Routes.TH
    -- ( ResourceTree(..),
    --   Piece(Dynamic, Static),
    --   FlatResource,
    --   Resource(resourceDispatch, resourceName, resourcePieces),
    --   Dispatch(Methods, Subsite) )

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!

genTypeScriptRoutes :: [ResourceTree String] -> FilePath -> IO ()
genTypeScriptRoutes resourcesApp fp = do
    createTree $ directory fp
    writeFile fp routesCs
  where
    routesCs =
        let res = (resToCoffeeString Nothing "" $ ResourceParent "paths" [] hackedTree)
        in  either id id (snd res)
            <> "\nvar PATHS:PATHS_TYPE_paths = new PATHS_TYPE_paths();"

    -- route hackery..
    fullTree = resourcesApp :: [ResourceTree String]
    landingRoutes = flip filter fullTree $ \case
        ResourceParent _ _ _ -> False
        ResourceLeaf res -> not $ elem (resourceName res) ["AuthR", "StaticR"]

    parentName :: String -> ResourceTree String -> Bool
    parentName name (ResourceParent n _ _) = n == name
    parentName _ _  = False

    parents =
        filter (\n -> parentName "PartialsH" n || parentName "ApiH" n) fullTree
    hackedTree = ResourceParent "staticPages" [] landingRoutes : parents
    cleanName = uncapitalize . dropWhileEnd isUpper
      where uncapitalize t = (toLower $ take 1 t) <> drop 1 t

    renderRoutePieces pieces = intercalate "/" $ map renderRoutePiece pieces
    renderRoutePiece p = case p of
        (_, Static st) -> pack st :: Text
        (_, Dynamic "Text") -> ":string"
        (_, Dynamic "Int") -> ":number"
        (_, Dynamic d) ->
            ":" <> pack (if isSuffixOf "Id" d then "string" else pack d)
    isVariable r = length r > 1 && DT.head r == ':'
    resRoute res = renderRoutePieces $ resourcePieces res
    resName res = cleanName . pack $ resourceName res
    lastName res = fromMaybe (resName res)
                 . find (not . isVariable)
                 . map renderRoutePiece
                 . reverse
                 . resourcePieces
                 $ res
    singleSlash = DT.replace "//" "/"
    resToCoffeeString :: Maybe Text -> Text -> ResourceTree String -> ([(Text, Text)], Either Text Text)
    resToCoffeeString _ routePrefix (ResourceLeaf res) =
        let rname = resName res in
        -- previously assumed there weren't multiple methods per route path
        -- now hacking in support
        let jsNames = case resourceDispatch res of
                Subsite _ _ -> error "subsite!"
                Methods _ [] -> error "no methods!"
                Methods _ methods ->
                    if length methods > 1 || rname == ""
                        then map (toLower . pack) methods
                        else [DT.replace "." "" $ lastName res]
        in ([], Right $ intercalate "\n" $ map mkLine jsNames)
      where
        pieces = DT.splitOn "/" routeString
        variables = snd $ foldl' (\(i,prev) typ -> (i+1, prev <> [("a" <> tshow i, typ)]))
                             (0::Int, [])
                             (filter isVariable  pieces)
        mkLine jsName = "  public " <> jsName <> "("
          <> csvArgs variables
          <> "):string { return " <> quote (routeStr variables variablePieces) <> "; }"

        routeStr vars ((Left p):rest) | null p    = routeStr vars rest
                                      | otherwise = "/" <> p <> routeStr vars rest
        routeStr (v:vars) ((Right _):rest) = "/' + " <> fst v <> " + '" <> routeStr vars rest
        routeStr [] [] = ""
        routeStr _ [] = error "extra vars!"
        routeStr [] _ = error "no more vars!"

        variablePieces = map (\p -> if isVariable p then Right p else Left p) pieces
        csvArgs :: [(Text, Text)] -> Text
        csvArgs = intercalate "," . map (\(var, typ) -> var <> typ)
        quote str = "'" <> str <> "'"
        routeString = singleSlash routePrefix <> resRoute res

    -- this is here because in the typescript code, we dont refer to
    -- PATHS.api.doc.foo but PATHS.doc.foobar.  so we can keep our route
    -- orgazniation in place but also leave TS alone
    resToCoffeeString parent routePrefix (ResourceParent "ApiH" pieces children) =
        (concatMap fst res, Left $ intercalate "\n" (map (either id id . snd) res))
      where
        fxn = resToCoffeeString parent (routePrefix <> "/" <> renderRoutePieces pieces <> "/")
        res = map fxn children

    resToCoffeeString parent routePrefix (ResourceParent name pieces children) =
        ([linkFromParent], Left $ resourceClassDef)
      where
        parentMembers f =
          intercalate "\n  " $ map f $ concatMap fst childTypescript
        memberInitFromParent (slot, klass) = "  this." <> slot <> " = new " <> klass <> "();"
        memberLinkFromParent (slot, klass) = "public " <> slot <> ": " <> klass <> ";"
        linkFromParent = (pref, resourceClassName)
        resourceClassDef = "class " <>  resourceClassName  <> " {\n"
          <> intercalate "\n" childMembers
          <> "  " <> parentMembers memberLinkFromParent
          <> "\n\n"
          <> "  constructor(){\n  "
          <> parentMembers memberInitFromParent
          <> "\n  }\n"
          <> "}\n\n"
          <> intercalate "\n" childClasses
        (childClasses, childMembers) = partitionEithers $ map snd childTypescript
        childTypescript = map fxn children
        jsName = maybe "" (<> "_") parent <> pref
        fxn = resToCoffeeString (Just jsName)
                    (routePrefix <> "/" <> renderRoutePieces pieces <> "/")
        pref = cleanName $ pack name
        resourceClassName = "PATHS_TYPE_" <> jsName

deriving instance (Show a) => Show (ResourceTree a)
deriving instance (Show a) => Show (FlatResource a)
