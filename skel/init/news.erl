-module(news).

-export([init/0]).

init() ->
    ok.
%%%%%%%%%%% Ideas
%    boss_news:watch_set("greetings",
%        fun(created, NewGreeting) ->
%                boss_mail:send("boss@evanmiller.org",
%                    "emmiller@gmail.com",
%                    "New greeting!",
%                    "There is a new greeting: ~p~n",
%                    [NewGreeting:greeting_text()])
%        end.
%    boss_news:watch("user-42.*",
%        fun
%            (Donald, 'location', OldLocation, NewLocation) ->
%                ;
%            (Donald, 'email_address', OldEmail, NewEmail)
%        end),
%
%    boss_news:watch("user-*.status",
%        fun(User, 'status', OldStatus, NewStatus) ->
%                Followers = User:followers(),
%                lists:map(fun(Follower) ->
%                            Follower:notify_status_update(User, NewStatus)
%                    end, Followers)
%        end),
%
%    boss_news:watch_set("users",
%        fun
%            (created, NewUser) ->
%                boss_mail:send(?WEBSITE_EMAIL_ADDRESS,
%                    ?ADMINISTRATOR_EMAIL_ADDRESS,
%                    "New account!",
%                    "~p just created an account!~n",
%                    [NewUser:name()]);
%            (deleted, OldUser) ->
%                ok
%        end),
%    
%    boss_news:watch_set("forum_replies",
%        fun
%            (created, Reply) ->
%                OrignalPost = Reply:original_post(),
%                OriginalAuthor = OriginalPost:author(),
%                case OriginalAuthor:is_online() of
%                    true ->
%                        boss_mq:push(OriginalAuthor:comet_channel(), <<"Someone replied!">>);
%                    false ->
%                        case OriginalAuthor:likes_email() of
%                            true ->
%                                boss_mail:send("website@blahblahblah",
%                                    OriginalAuthor:email_address(),
%                                    "Someone replied!"
%                                    "~p has replied to your post on ~p~n",
%                                    [(Reply:author()):name(), OriginalPost:title()]);
%                            false ->
%                                ok
%                        end
%                end;
%            (_ _) -> ok
%        end),
%    
%    boss_news:watch_set("forum_categories",
%        fun
%            (created, NewCategory) ->
%                boss_mail:send(?WEBSITE_EMAIL_ADDRESS,
%                    ?ADMINISTRATOR_EMAIL_ADDRESS,
%                    "New category: "++NewCategory:name(),
%                    "~p has created a new forum category called \"~p\"~n",
%                    [(NewCategory:created_by()):name(), NewCategory:name()]);
%            (_, _) -> ok
%        end),
%
%    boss_news:watch("forum_category-*.is_deleted",
%        fun 
%            (ForumCategory, 'is_deleted', false, true) ->
%                ;
%            (ForumCategory, 'is_deleted', true, false) ->
%        end).

%boss_news:deleted("person-42", OldAttrs),
%boss_news:updated("person-42", OldAttrs, NewAttrs),
%boss_news:created("person-42", NewAttrs)

% POST /admin/news_api/deleted/person-42
% old[status] = something

% POST /admin/news_api/updated/person-42
% old[status] = blah
% new[status] = barf

% POST /admin/news_api/created/person-42
% new[status] = something
