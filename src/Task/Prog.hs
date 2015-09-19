module Reporting.Progress where


type Package = (Pkg.Name, Pkg.Version)


data Model = Model
    { _activeFetch :: Maybe Package
    ,
    | BuildDependencies


data Event
    = Start State
    | End

    -- FetchDependencies
    | FetchDependency
        { _cached :: Bool
        , _name ::
        }


update :: Event -> State -> State
update event state =
  case (state, event) of
    (None, Start newState) ->
        newState

    (None, End) ->
        error "invalid state transition"

    (FetchDependencies, End) ->


    (FetchDependencies, FetchDependencies cached name version) ->
        putStrLn (Pkg.toString name ++ " " ++ Pkg.versionToString version)