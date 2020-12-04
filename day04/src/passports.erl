-module(passports).

%% API exports
-export([parse/1, validate/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
parse(Line) ->
    FieldStrings = string:split(Line, " ", all),
    lists:foldr(fun(FieldStr, FieldsMap) ->
                        [Key, Value | _] = string:split(FieldStr, ":", all),
                        maps:put(Key, Value, FieldsMap)
                end, #{}, FieldStrings).

validate(#{"byr" := BirthYear, "iyr" := IssueYear, "eyr" := ExpirationYear, "hgt" := Height, "hcl" := HairColor, "ecl" := EyeColor, "pid" := PassportID}) ->
    is_valid_birth_year(BirthYear) and
    is_valid_issue_year(IssueYear) and
    is_valid_expiration_year(ExpirationYear) and
    is_valid_height(Height) and
    is_valid_hair_color(HairColor) and
    is_valid_eye_color(EyeColor) and
    is_valid_passport_id(PassportID);
validate(_) ->
    false.

%%====================================================================
%% Internal functions
%%====================================================================

is_valid_birth_year(Str) ->
    is_in_range(Str, 1920, 2002).

is_valid_issue_year(Str) ->
    is_in_range(Str, 2010, 2020).

is_valid_expiration_year(Str) ->
    is_in_range(Str, 2020, 2030).

is_in_range(Str, Min, Max) ->
    N = list_to_integer(Str),
    (N >= Min) and (N =< Max).

is_valid_height(Str) ->
    case string:to_integer(Str) of
        {N, "cm"} -> (N >= 150) and (N =< 193);
        {N, "in"} -> (N >= 59) and (N =< 76);
        _Else -> false
    end.

is_valid_hair_color(Str) ->
    matches_regex(Str, "^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$").

is_valid_eye_color(Str) ->
    sets:is_element(Str, sets:from_list(["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])).

is_valid_passport_id(Str) ->
    matches_regex(Str, "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$").

matches_regex(Str, Regex) ->
    re:run(Str, Regex) =/= nomatch.
