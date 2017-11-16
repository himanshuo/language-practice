module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury x = (ageOn Earth x) / 0.2408467
ageOn Venus x = (ageOn Earth x) / 0.61519726
ageOn Earth x = x / 31557600.0
ageOn Mars x = (ageOn Earth x) / 1.8808158
ageOn Jupiter x = (ageOn Earth x) / 11.862615
ageOn Saturn x = (ageOn Earth x) / 29.447498
ageOn Uranus x = (ageOn Earth x) / 84.016846
ageOn Neptune x = (ageOn Earth x) / 164.79132

    -- Earth: orbital period 365.25 Earth days, or 31557600 seconds
    -- Mercury: orbital period 0.2408467 Earth years
    -- Venus: orbital period 0.61519726 Earth years
    -- Mars: orbital period 1.8808158 Earth years
    -- Jupiter: orbital period 11.862615 Earth years
    -- Saturn: orbital period 29.447498 Earth years
    -- Uranus: orbital period 84.016846 Earth years
    -- Neptune: orbital period 164.79132 Earth years
    