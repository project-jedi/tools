unit MediaWikiUtils;

interface

uses
  SysUtils,
  Classes,
  Math,
  JclBase,
  JclSimpleXml;

type
  EMediaWikiException = class(Exception)
  private
    FInfo: string;
  public
    constructor Create(const AInfo: string);
    procedure AfterConstruction; override;
    property Info: string read FInfo;
  end;

  EMediaWikiWarning = class(EMediaWikiException)
  private
    FQuery: string;
  public
    constructor Create(const AInfo, AQuery: string);
    procedure AfterConstruction; override;
    property Query: string read FQuery;
  end;

  EMediaWikiError = class(EMediaWikiException)
  private
    FCode: string;
  public
    constructor Create(const AInfo, ACode: string);
    procedure AfterConstruction; override;
    property Code: string read FCode;
  end;

  TMediaWikiID = Integer;

type
  TMediaWikiXMLWarningCallback = procedure (const AInfo, AQuery: string) of object;
  TMediaWikiXMLErrorCallback = procedure (const AInfo, ACode: string) of object;

procedure MediaWikiCheckXML(XML: TJclSimpleXML; WarningCallback: TMediaWikiXMLWarningCallback;
  ErrorCallback: TMediaWikiXMLErrorCallback);

type
  TMediaWikiOutputFormat = (mwoJSON,      // JSON format
                            mwoPHP,       // serialized PHP format
                            mwoWDDX,      // WDDX format
                            mwoXML,       // XML format
                            mwoYAML,      // YAML format
                            mwoDebugJSON, // JSON format with the debugging elements (HTML)
                            mwoText,      // PHP print_r() format
                            mwoDebug);    // PHP var_export() format

  TMediaWikiLoginResult = (mwlNoName, // You didn't set the lgname parameter
                           mwlIllegal, // You provided an illegal username
                           mwlNotExists, // The username you provided doesn't exist
                           mwlEmptyPass, // You didn't set the lgpassword parameter or you left it empty
                           mwlWrongPass, // The password you provided is incorrect
                           mwlWrongPluginPass, // Same as WrongPass, returned when an authentication plugin rather than MediaWiki itself rejected the password
                           mwlCreateBlocked, // The wiki tried to automatically create a new account for you, but your IP address has been blocked from account creation
                           mwlThrottled, // You've logged in too many times in a short time. See also throttling
                           mwlBlocked, // User is blocked
                           mwlMustBePosted, // The login module requires a POST request
                           mwlNeedToken, // Either you did not provide the login token or the sessionid cookie. Request again with the token and cookie given in this response
                           mwlSuccess); // Login success

const
  MediaWikiOutputFormats: array [TMediaWikiOutputFormat] of string  =
    ( 'json',   // JSON format
      'php',    // serialized PHP format
      'wddx',   // WDDX format
      'xml',    // XML format
      'yaml',   // YAML format
      'rawfm',  // JSON format with the debugging elements (HTML)
      'txt',    // PHP print_r() format
      'dbg' );  // PHP var_export() format

  MediaWikiLoginResults: array [TMediaWikiLoginResult] of string =
    ( 'NoName',          // You didn't set the lgname parameter
      'Illegal',         // You provided an illegal username
      'NotExists',       // The username you provided doesn't exist
      'EmptyPass',       // You didn't set the lgpassword parameter or you left it empty
      'WrongPass',       // The password you provided is incorrect
      'WrongPluginPass', // Same as WrongPass, returned when an authentication plugin rather than MediaWiki itself rejected the password
      'CreateBlocked',   // The wiki tried to automatically create a new account for you, but your IP address has been blocked from account creation
      'Throttled',       // You've logged in too many times in a short time. See also throttling
      'Blocked',         // User is blocked
      'mustbeposted',    // The login module requires a POST request
      'NeedToken',       // Either you did not provide the login token or the sessionid cookie. Request again with the token and cookie given in this response
      'Success' );       // Login success

function FindMediaWikiLoginResult(const AString: string): TMediaWikiLoginResult;

function StrISO8601ToDateTime(const When: string): TDateTime;
function DateTimeToStrISO8601(When: TDateTime): string;

// post stuff
procedure MediaWikiQueryAdd(Queries: TStrings; const AName: string; const AValue: string = ''; RawValue: Boolean = False);
function MediaWikiQueryPost(Queries: TStrings): AnsiString;

// login stuff
procedure MediaWikiQueryLoginAdd(Queries: TStrings; const lgName, lgPassword, lgToken: string; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryLoginParseXmlResult(XML: TJclSimpleXML; out LoginResult: TMediaWikiLoginResult;
  out LoginUserID: TMediaWikiID; out LoginUserName, LoginToken, CookiePrefix, SessionID: string);

// logout stuff
procedure MediaWikiQueryLogoutAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryLogoutParseXmlResult(XML: TJclSimpleXML);

// query, site info general
procedure MediaWikiQuerySiteInfoGeneralAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoGeneralParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info namespaces
procedure MediaWikiQuerySiteInfoNamespacesAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoNamespacesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info, namespace aliases
procedure MediaWikiQuerySiteInfoNamespaceAliasesAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoNamespaceAliasesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info, special page aliases
procedure MediaWikiQuerySiteInfoSpecialPageAliasesAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoSpecialPageAliasesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info, magic words
procedure MediaWikiQuerySiteInfoMagicWordsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoMagicWordsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info, statistics
procedure MediaWikiQuerySiteInfoStatisticsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoStatisticsParseXmlResult(XML: TJclSimpleXML; Info: TStrings);

// query, site info, inter wiki map
procedure MediaWikiQuerySiteInfoInterWikiMapAdd(Queries: TStrings; Local: Boolean; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoInterWikiMapParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info , DB replication lag
procedure MediaWikiQuerySiteInfoDBReplLagAdd(Queries: TStrings; ShowAllDB: Boolean;
  OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoDBReplLagParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info, user groups
procedure MediaWikiQuerySiteInfoUserGroupsAdd(Queries: TStrings; IncludeUserCount: Boolean;
  OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoUserGroupsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, site info, extensions
type
  TMediaWikiExtension = record
    ExtensionType: string;
    ExtensionName: string;
    ExtensionDescription: string;
    ExtensionDescriptionMsg: string;
    ExtensionAuthor: string;
    ExtensionVersion: string;
  end;
  TMediaWikiExtensions = array of TMediaWikiExtension;

procedure MediaWikiQuerySiteInfoExtensionsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQuerySiteInfoExtensionsParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiExtensions);

// query, user info, block info
procedure MediaWikiQueryUserInfoBlockInfoAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoBlockInfoParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, user info, has msg
procedure MediaWikiQueryUserInfoHasMsgAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoHasMsgParseXmlResult(XML: TJclSimpleXML; out HasMessage: Boolean);

// query, user info, groups
procedure MediaWikiQueryUserInfoGroupsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoGroupsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

//query, user info, rights
procedure MediaWikiQueryUserInfoRightsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoRightsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, user info, changeable groups
procedure MediaWikiQueryUserInfoChangeableGroupsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoChangeableGroupsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, user info, options
procedure MediaWikiQueryUserInfoOptionsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoOptionsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, user info, edit count
procedure MediaWikiQueryUserInfoEditCountAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoEditCountParseXmlResult(XML: TJclSimpleXML; out EditCount: Integer);

// query, user info, rate limits
type
  TMediaWikiRateLimit = record
    RateLimitAction: string;
    RateLimitGroup: string;
    RateLimitHits: Integer;
    RateLimitSeconds: Integer;
  end;
  TMediaWikiRateLimits = array of TMediaWikiRateLimit;

procedure MediaWikiQueryUserInfoRateLimitsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryUserInfoRateLimitsParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiRateLimits);

// query, messages
procedure MediaWikiQueryMessagesAdd(Queries: TStrings; const NameFilter, ContentFilter, Lang: string;
  OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryMessagesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);

// query, page info
type
  TMediaWikiPageBasics = record
    PageID: TMediaWikiID;
    PageNamespace: Integer;
    PageTitle: string;
  end;

  TMediaWikiPageProtection = record
    PageProtectionAction: string;
    PageProtectionGroup: string;
    PageProtectionExpiry: string;
  end;
  TMediaWikiPageProtections = array of TMediaWikiPageProtection;

  TMediaWikiPageFlag = (mwfPageIsNew, mwfPageIsRedirect);
  TMediaWikiPageFlags = set of TMediaWikiPageFlag;

  TMediaWikiPageInfo = record
    PageBasics: TMediaWikiPageBasics;
    PageLastTouched: TDateTime;
    PageRevisionID: TMediaWikiID;
    PageViews: Integer;
    PageSize: Integer;
    PageFlags: TMediaWikiPageFlags;
    PageProtections: TMediaWikiPageProtections; // on request, use mwfIncludeProtection
    PageTalkID: TMediaWikiID;                   // on request, use mwfIncludeTalkID
    PageSubjectID: TMediaWikiID;                // on request, mwfIncludeSubjectID
    PageFullURL: string;                        // on request, mwfIncludeURL
    PageEditURL: string;                        // on request, mwfIncludeURL
    PageEditToken: string;                      // on request, mwfIncludeEditToken
    PageMoveToken: string;                      // on request, mwfIncludeMoveToken
    PageDeleteToken: string;                    // on request, mwfIncludeDeleteToken
  end;
  TMediaWikiPageInfos = array of TMediaWikiPageInfo;

  TMediaWikiPageInfoFlag = (mwfIncludeProtection, mwfIncludeTalkID, mwfIncludeSubjectID,
    mwfIncludeURL, mwfIncludeEditToken, mwfIncludeMoveToken, mwfIncludeDeleteToken);
  TMediaWikiPageInfoFlags = set of TMediaWikiPageInfoFlag;

procedure MediaWikiQueryPageInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean; Flags: TMediaWikiPageInfoFlags;
  OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryPageInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageInfos);

// query, page info, revision info
type
  TMediaWikiPageRevisionFlag = (mwfMinorEdit);
  TMediaWikiPageRevisionFlags = set of TMediaWikiPageRevisionFlag;

  TMediaWikiPageRevisionInfo = record
    PageRevisionInfoPageBasics: TMediaWikiPageBasics;
    PageRevisionInfoID: TMediaWikiID;
    PageRevisionInfoFlags: TMediaWikiPageRevisionFlags;
    PageRevisionInfoDateTime: TDateTime;
    PageRevisionInfoAuthor: string;
    PageRevisionInfoComment: string;
    PageRevisionInfoSize: Integer;
    PageRevisionInfoContent: string;
    // TODO RevisionTags: string;
    PageRevisionInfoRollbackToken: string;
  end;
  TMediaWikiPageRevisionInfos = array of TMediaWikiPageRevisionInfo;

  TMediaWikiPageRevisionInfoFlag = (mwfIncludeRevisionID, mwfIncludeRevisionFlags, mwfIncludeRevisionTimeStamp,
    mwfIncludeRevisionAuthor, mwfIncludeRevisionComment, mwfIncludeRevisionSize, mwfIncludeRevisionContent,
    mwfIncludeRevisionRollbackToken, mwfRevisionReverseOrder, mwfRevisionContentXml, mwfRevisionContentExpandTemplates,
    mwfRevisionContinue);
  TMediaWikiPageRevisionInfoFlags = set of TMediaWikiPageRevisionInfoFlag;

procedure MediaWikiQueryPageRevisionInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  Flags: TMediaWikiPageRevisionInfoFlags; MaxRevisions, Section: Integer; StartRevisionID, EndRevisionID: TMediaWikiID;
  const StartDateTime, EndDateTime: TDateTime; const IncludeUser, ExcludeUser: string; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryPageRevisionInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageRevisionInfos); overload;
procedure MediaWikiQueryPageRevisionInfoParseXmlResult(XML: TJclSimpleXML; out StartID, EndID: TMediaWikiID); overload;

// query, page info, category info
type
  TMediaWikiPageCategoryInfo = record
    CategoryPageBasics: TMediaWikiPageBasics;
    CategoryTitle: string;
    CategoryNameSpace: Integer;
    CategoryTimeStamp: TDateTime; // on request
    CategorySortKey: string;      // on request
  end;
  TMediaWikiPageCategoryInfos = array of TMediaWikiPageCategoryInfo;

  TMediaWikiPageCategoryInfoFlag = (mwfIncludeCategorySortKey, mwfIncludeCategoryTimeStamp,
    mwfCategoryHidden);
  TMediaWikiPageCategoryInfoFlags = set of TMediaWikiPageCategoryInfoFlag;

procedure MediaWikiQueryPageCategoryInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  Flags: TMediaWikiPageCategoryInfoFlags; MaxCategories: Integer;
  const CategoryTitles, CategoryStart: string; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryPageCategoryInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageCategoryInfos); overload;
procedure MediaWikiQueryPageCategoryInfoParseXmlResult(XML: TJclSimpleXML; out StartCategory: string); overload;

// query, page info, link info
type
  TMediaWikiPageLinkInfo = record
    LinkSourceBasics: TMediaWikiPageBasics;
    LinkTargetTitle: string;
    LinkTargetNameSpace: Integer;
  end;
  TMediaWikiPageLinkInfos = array of TMediaWikiPageLinkInfo;

procedure MediaWikiQueryPageLinkInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  MaxLinks, Namespace: Integer; const LinkStart: string; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryPageLinkInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageLinkInfos); overload;
procedure MediaWikiQueryPageLinkInfoParseXmlResult(XML: TJclSimpleXML; out StartCategory: string); overload;

// query, page info, template info
type
  TMediaWikiPageTemplateInfo = record
    TemplatePageBasics: TMediaWikiPageBasics;
    TemplateTitle: string;
    TemplateNameSpace: Integer;
  end;
  TMediaWikiPageTemplateInfos = array of TMediaWikiPageTemplateInfo;

procedure MediaWikiQueryPageTemplateInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  MaxTemplates, Namespace: Integer; TemplateStart: string; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryPageTemplateInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageTemplateInfos); overload;
procedure MediaWikiQueryPageTemplateInfoParseXmlResult(XML: TJclSimpleXML; out StartTemplate: string); overload;

// query, page info, ext links
type
  TMediaWikiPageExtLinkInfo = record
    ExtLinkPageBasics: TMediaWikiPageBasics;
    ExtLinkTarget: string;
  end;
  TMediaWikiPageExtLinkInfos = array of TMediaWikiPageExtLinkInfo;

procedure MediaWikiQueryPageExtLinkInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  MaxLinks: Integer; const StartLink: string; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryPageExtLinkInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageExtLinkInfos); overload;
procedure MediaWikiQueryPageExtLinkInfoParseXmlResult(XML: TJclSimpleXML; out StartExtLink: string); overload;

// query, list, all pages
type
  TMediaWikiAllPageInfos = array of TMediaWikiPageBasics;

  TMediaWikiAllPageFilterRedir = (mwfAllPageFilterAll, mwfAllPageFilterRedirect, mwfAllPageFilterNonRedirect);
  TMediaWikiAllPageFilterLang = (mwfAllPageLangAll, mwfAllPageLangOnly, mwfAllPageLangNone);
  TMediaWikiAllPageFilterProtection = (mwfAllPageProtectionNone, mwfAllPageProtectionEdit, mwfAllPageProtectionMove);
  TMediaWikiAllPageFilterLevel = (mwfAllPageLevelNone, mwfAllPageLevelAutoConfirmed, mwfAllPageLevelSysops);
  TMediaWikiAllPageDirection = (mwfAllPageAscending, mwfAllPageDescending);

procedure MediaWikiQueryAllPageAdd(Queries: TStrings; const StartPage, Prefix: string; MaxPage: Integer;
  Namespace: Integer; RedirFilter: TMediaWikiAllPageFilterRedir;
  LangFilter: TMediaWikiAllPageFilterLang; MinSize, MaxSize: Integer; ProtectionFilter: TMediaWikiAllPageFilterProtection;
  LevelFilter: TMediaWikiAllPageFilterLevel; Direction: TMediaWikiAllPageDirection; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryAllPageParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiAllPageInfos); overload;
procedure MediaWikiQueryAllPageParseXmlResult(XML: TJclSimpleXML; out StartPage: string); overload;

// query, list, alllinks: Returns a list of (unique) links to pages in a given namespace starting ordered by link title.
type
  TMediaWikiAllLinkInfo = record
    LinkTitle: string;
    PageID: TMediaWikiID;
    LinkNamespace: Integer;
  end;
  TMediaWikiAllLinkInfos = array of TMediaWikiAllLinkInfo;

  TMediaWikiAllLinkInfoFlag = (mwfLinkUnique, mwfLinkIncludePageID);
  TMediaWikiAllLinkInfoFlags = set of TMediaWikiAllLinkInfoFlag;

procedure MediaWikiQueryAllLinkAdd(Queries: TStrings; const StartLink, Prefix: string; MaxLink: Integer;
  const ContinueLink: string; Namespace: Integer; Flags: TMediaWikiAllLinkInfoFlags;
  OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryAllLinkParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiAllLinkInfos); overload;
procedure MediaWikiQueryAllLinkParseXmlResult(XML: TJclSimpleXML; out StartPage: string); overload;

type
  TMediaWikiAllCategoryInfoFlag = (mwfCategoryDescending);
  TMediaWikiAllCategoryInfoFlags = set of TMediaWikiAllCategoryInfoFlag;

procedure MediaWikiQueryAllCategoryAdd(Queries: TStrings; const StartCategory, Prefix: string; MaxCategory: Integer;
  Flags: TMediaWikiAllCategoryInfoFlags; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryAllCategoryParseXmlResult(XML: TJclSimpleXML; Infos: TStrings); overload;
procedure MediaWikiQueryAllCategoryParseXmlResult(XML: TJclSimpleXML; out StartCategory: string); overload;

type
  TMediaWikiAllUserInfo = record
    UserName: string;
    UserGroups: TDynStringArray;
    UserEditCount: Integer;
    UserRegistration: TDateTime;
  end;
  TMediaWikiAllUserInfos = array of TMediaWikiAllUserInfo;

  TMediaWikiAllUserInfoFlag = (mwfIncludeUserEditCount, mwfIncludeUserGroups, mwfIncludeUserRegistration);
  TMediaWikiAllUserInfoFlags = set of TMediaWikiAllUserInfoFlag;

procedure MediaWikiQueryAllUserAdd(Queries: TStrings; const StartUser, Prefix, Group: string; MaxUser: Integer;
  Flags: TMediaWikiAllUserInfoFlags; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryAllUserParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiAllUserInfos); overload;
procedure MediaWikiQueryAllUserParseXmlResult(XML: TJclSimpleXML; out StartUser: string); overload;

type
  TMediaWikiBackLinkFlag = (mwfBackLinkIsRedirect,  // BackLinkPageID is redirected to BackLinkTitle
                            mwfBackLinkToRedirect); // BackLinkFromPageID links to BackLinkPageID which is redirected to BackLinkTitle
  TMediaWikiBackLinkFlags = set of TMediaWikiBackLinkFlag;

  TMediaWikiBackLinkInfo = record
    BackLinkPageBasics: TMediaWikiPageBasics;
    BackLinkFlags: TMediaWikiBackLinkFlags;
    BackLinkRedirFromPageBasics: TMediaWikiPageBasics;
  end;
  TMediaWikiBackLinkInfos = array of TMediaWikiBackLinkInfo;

  TMediaWikiBackLinkInfoFlag = (mwfExcludeBackLinkRedirect, mwfExcludeBackLinkNonRedirect, mwfIncludeBackLinksFromRedirect);
  TMediaWikiBackLinkInfoFlags = set of TMediaWikiBackLinkInfoFlag;

procedure MediaWikiQueryBackLinkAdd(Queries: TStrings; const BackLinkTitle: string; Namespace, MaxLink: Integer; const StartBackLink: string;
  Flags: TMediaWikiBackLinkInfoFlags; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryBackLinkParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiBackLinkInfos); overload;
procedure MediaWikiQueryBackLinkParseXmlResult(XML: TJclSimpleXML; out StartBackLink: string); overload;

type
  TMediaWikiBlockFlag = (mwfBlockAutomatic, mwfBlockAnonymousEdits, mwfBlockNoAccountCreate, mwfBlockAutomaticBlocking, mwfBlockNoEmail, mwfBlockHidden);
  TMediaWikiBlockFlags = set of TMediaWikiBlockFlag;

  TMediaWikiBlockInfo = record
    BlockID: TMediaWikiID;
    BlockUser: string;
    BlockUserID: TMediaWikiID;
    BlockByUser: string;
    BlockByUserID: TMediaWikiID;
    BlockDateTime: TDateTime;
    BlockExpirityDateTime: TDateTime;
    BlockReason: string;
    BlockIPRangeStart: string;
    BlockIPRangeStop: string;
    BlockFlags: TMediaWikiBlockFlags;
  end;
  TMediaWikiBlockInfos = array of TMediaWikiBlockInfo;

  TMediaWikiBlockInfoFlag = (mwfBlockID, mwfBlockUser, mwfBlockByUser, mwfBlockDateTime, mwfBlockExpiry, mwfBlockReason, mwfBlockIPRange, mwfBlockFlags, mwfBlockDescending);
  TMediaWikiBlockInfoFlags = set of TMediaWikiBlockInfoFlag;

procedure MediaWikiQueryBlockAdd(Queries: TStrings; const StartDateTime, StopDateTime: TDateTime;
  const BlockIDs, Users, IP: string; MaxBlock: Integer; const StartBlock: string;
  Flags: TMediaWikiBlockInfoFlags; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryBlockParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiBlockInfos); overload;
procedure MediaWikiQueryBlockParseXmlResult(XML: TJclSimpleXML; out StartBlock: string); overload;

type
  TMediaWikiCategoryMemberInfo = record
    CategoryMemberPageBasics: TMediaWikiPageBasics;
    CategoryMemberDateTime: TDateTime;
    CategoryMemberSortKey: string;
  end;
  TMediaWikiCategoryMemberInfos = array of TMediaWikiCategoryMemberInfo;

  TMediaWikiCategoryMemberInfoFlag = (mwfCategoryMemberPageID, mwfCategoryMemberPageTitle,
    mwfCategoryMemberPageDateTime, mwfCategoryMemberPageSortKey, mwfCategoryMemberDescending);
  TMediaWikiCategoryMemberInfoFlags = set of TMediaWikiCategoryMemberInfoFlag;

procedure MediaWikiQueryCategoryMemberAdd(Queries: TStrings; const CategoryTitle: string; PageNamespace: Integer;
  const StartDateTime, StopDateTime: TDateTime; const StartSortKey, StopSortKey: string;
  MaxCategoryMember: Integer; const StartCategoryMember: string;
  Flags: TMediaWikiCategoryMemberInfoFlags; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiQueryCategoryMemberParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiCategoryMemberInfos); overload;
procedure MediaWikiQueryCategoryMemberParseXmlResult(XML: TJclSimpleXML; out StartCategoryMember: string); overload;

type
  TMediaWikiEditFlag = (mwfEditMinor, mwfEditNotMinor, mwfEditBot, mwfEditAlwaysRecreate, mwfEditMustCreate, mwfEditMustExist,
    mwfEditWatchAdd, mwfEditWatchRemove, mwfEditWatchNoChange, mwfEditUndoAfterRev);
  TMediaWikiEditFlags = set of TMediaWikiEditFlag;

  TMediaWikiEditInfo = record
    EditSuccess: Boolean;
    EditPageTitle: string;
    EditPageID: TMediaWikiID;
    EditOldRevID: TMediaWikiID;
    EditNewRevID: TMediaWikiID;
    EditCaptchaType: string;
    EditCaptchaURL: string;
    EditCaptchaMime: string;
    EditCaptchaID: string;
    EditCaptchaQuestion: string;
  end;

procedure MediaWikiEditAdd(Queries: TStrings; const PageTitle, Section, Text, PrependText, AppendText, EditToken, Summary, MD5, CaptchaID, CaptchaWord: string;
  const BaseDateTime, StartDateTime: TDateTime; UndoRevisionID: TMediaWikiID;
  Flags: TMediaWikiEditFlags; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiEditParseXmlResult(XML: TJclSimpleXML; out Info: TMediaWikiEditInfo);

type
  TMediaWikiMoveFlag = (mwfMoveTalk, mwfMoveSubPages, mwfMoveNoRedirect, mwfMoveAddToWatch, mwfMoveNoWatch);
  TMediaWikiMoveFlags = set of TMediaWikiMoveFlag;

  TMediaWikiMoveInfo = record
    MoveSuccess: Boolean;
    MoveFromPage: string;
    MoveToPage: string;
    MoveReason: string;
    MoveFromTalk: string;
    MoveToTalk: string;
  end;

procedure MediaWikiMoveAdd(Queries: TStrings; const FromPageTitle, ToPageTitle, MoveToken, Reason: string;
  FromPageID: TMediaWikiID; Flags: TMediaWikiMoveFlags; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiMoveParseXmlResult(XML: TJclSimpleXML; out Info: TMediaWikiMoveInfo);

type
  TMediaWikiDeleteInfo = record
    DeleteSuccess: Boolean;
    DeletePage: string;
    DeleteReason: string;
  end;

procedure MediaWikiDeleteAdd(Queries: TStrings; const PageTitle, DeleteToken, Reason: string;
  PageID: TMediaWikiID; OutputFormat: TMediaWikiOutputFormat);
procedure MediaWikiDeleteParseXmlResult(XML: TJclSimpleXML; out Info: TMediaWikiDeleteInfo);

implementation

uses
  DateUtils,
  OverbyteIcsUrl,
  JclStrings;

//=== { EMediaWikiException } ================================================

constructor EMediaWikiException.Create(const AInfo: string);
begin
  inherited Create('');
  FInfo := AInfo;
end;

procedure EMediaWikiException.AfterConstruction;
begin
  Message := Format('MediaWiki exception with message: "%s"', [Info]);
end;

//=== { EMediaWikiWarning } ==================================================

constructor EMediaWikiWarning.Create(const AInfo, AQuery: string);
begin
  inherited Create(AInfo);
  FQuery := AQuery;
end;

procedure EMediaWikiWarning.AfterConstruction;
begin
  Message := Format('MediaWiki warning during query "%s" with info "%s"', [Query, Info]);
end;

//=== { EMediaWikiError } ====================================================

constructor EMediaWikiError.Create(const AInfo, ACode: string);
begin
  inherited Create(AInfo);
  FCode := ACode;
end;

procedure EMediaWikiError.AfterConstruction;
begin
  Message := Format('MediaWiki error code "%s" with info "%s"', [Code, Info]);
end;

procedure MediaWikiCheckXML(XML: TJclSimpleXML; WarningCallback: TMediaWikiXMLWarningCallback;
  ErrorCallback: TMediaWikiXMLErrorCallback);
var
  ErrorElem, WarningsElem, WarningElem: TJclSimpleXMLElem;
  Info, Code: string;
  Index: Integer;
begin
  XML.Options := XML.Options - [sxoAutoCreate];
  // check errors and warnings
  ErrorElem := XML.Root.Items.ItemNamed['error'];
  WarningsElem := XML.Root.Items.ItemNamed['warnings'];
  if Assigned(ErrorElem) then
  begin
    XML.Options := XML.Options + [sxoAutoCreate];
    Info := ErrorElem.Properties.ItemNamed['info'].Value;
    Code := ErrorElem.Properties.ItemNamed['code'].Value;
    ErrorCallback(Info, Code);
  end;
  if Assigned(WarningsElem) then
  begin
    XML.Options := XML.Options - [sxoAutoCreate];
    for Index := 0 to WarningsElem.Items.Count - 1 do
    begin
      WarningElem := WarningsElem.Items.Item[Index];
      WarningCallback(WarningElem.Value, WarningElem.Name);
    end;
  end;
end;

function FindMediaWikiLoginResult(const AString: string): TMediaWikiLoginResult;
begin
  for Result := Low(TMediaWikiLoginResult) to High(TMediaWikiLoginResult) do
    if SameText(AString, MediaWikiLoginResults[Result]) then
      Exit;
  raise EMediaWikiException.Create('not a valid login result');
end;

function StrISO8601ToDateTime(const When: string): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec: Integer;
  ErrCode: Integer;
begin
  Result := 0;
  if (Length(When) = 20) and (When[5] = '-') and (When[8] = '-') and (When[11] = 'T') and
    (When[14] = ':') and (When[17] = ':') and (When[20] = 'Z') then
  begin
    Val(Copy(When, 1, 4), Year, ErrCode);
    if (ErrCode <> 0) or (Year < 0) then
      Exit;
    Val(Copy(When, 6, 2), Month, ErrCode);
    if (ErrCode <> 0) or (Month < 1) or (Month > 12) then
      Exit;
    Val(Copy(When, 9, 2), Day, ErrCode);
    if (ErrCode <> 0) or (Day < 1) or (Day > 31) then
      Exit;
    Val(Copy(When, 12, 2), Hour, ErrCode);
    if (ErrCode <> 0) or (Hour < 0) or (Hour > 23) then
      Exit;
    Val(Copy(When, 15, 2), Min, ErrCode);
    if (ErrCode <> 0) or (Min < 0) or (Min > 59) then
      Exit;
    Val(Copy(When, 18, 2), Sec, ErrCode);
    if (ErrCode <> 0) or (Sec < 0) or (Sec > 59) then
      Exit;

    Result := DateUtils.EncodeDateTime(Year, Month, Day, Hour, Min, Sec, 0);
  end
  else
  if When = 'infinity' then
    Result := Infinity;
end;

function DateTimeToStrISO8601(When: TDateTime): string;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  if When = Infinity then
    Result := 'infinity'
  else
  begin
    DateUtils.DecodeDateTime(When, Year, Month, Day, Hour, Min, Sec, MSec);
    Result := Format('%.4d-%.2d-%.2dT%.2d:%.2d:%.2dZ', [Year, Month, Day, Hour, Min, Sec]);
  end;
end;

// post stuff
procedure MediaWikiQueryAdd(Queries: TStrings; const AName, AValue: AnsiString; RawValue: Boolean);
var
  NamePos: Integer;
begin
  NamePos := Queries.IndexOfName(string(AName));
  if (NamePos >= 0) and (AValue <> '') then
  begin
    Queries.Values[AName] := Queries.Values[AName] + '|' + AValue;
    if RawValue then
      Queries.Objects[NamePos] := TObject(1);
  end
  else
  if AValue = '' then
  begin
    Queries.Values[AName] := 'true';
    if RawValue then
    begin
      NamePos := Queries.IndexOfName(AName);
      Queries.Objects[NamePos] := TObject(1);
    end;
  end
  else
  begin
    Queries.Values[AName] := AValue;
    if RawValue then
    begin
      NamePos := Queries.IndexOfName(AName);
      Queries.Objects[NamePos] := TObject(1);
    end;
  end;
end;

function MediaWikiQueryPost(Queries: TStrings): AnsiString;
var
  AName: string;
  I, J: Integer;
  Values: TStrings;
begin
  Result := '';
  Values := TStringList.Create;
  try
    for I := 0 to Queries.Count - 1 do
    begin
      if Result <> '' then
        Result := Result + '&';
      AName := Queries.Names[I];
      if AName = '' then
        Result := Result + AnsiString(Queries.Strings[I])
      else
      begin
        if Queries.Objects[I] = nil then
        begin
          // call UrlEncodeToA
          StrToStrings(Queries.ValueFromIndex[I], '|', Values, False);
          for J := 0 to Values.Count - 1 do
            Values.Strings[J] := UrlEncodeToA(Values.Strings[J]);
          Result := Result + AnsiString(AName) + '=' + AnsiString(StringsToStr(Values, '|', False));
        end
        else
          // skip UrlEncodeToA
          Result := Result + AnsiString(Queries.Strings[I]);
      end;
    end;
  finally
    Values.Free;
  end;
end;

// login stuff
procedure MediaWikiQueryLoginAdd(Queries: TStrings; const lgName, lgPassword, lgToken: string;
  OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'login');
  MediaWikiQueryAdd(Queries, 'lgname', lgName);
  MediaWikiQueryAdd(Queries, 'lgpassword', lgPassword);
  if lgToken <> '' then
    MediaWikiQueryAdd(Queries, 'lgtoken', lgToken);
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryLoginParseXmlResult(XML: TJclSimpleXML; out LoginResult: TMediaWikiLoginResult;
  out LoginUserID: TMediaWikiID; out LoginUserName, LoginToken, CookiePrefix, SessionID: string);
var
  Login: TJclSimpleXMLElem;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Login := XML.Root.Items.ItemNamed['login'];
  LoginResult := FindMediaWikiLoginResult(Login.Properties.ItemNamed['result'].Value);
  LoginUserID := Login.Properties.ItemNamed['lguserid'].IntValue;
  LoginUserName := Login.Properties.ItemNamed['lgusername'].Value;
  LoginToken := Login.Properties.ItemNamed['lgtoken'].Value;
  CookiePrefix := Login.Properties.ItemNamed['cookieprefix'].Value;
  SessionID := Login.Properties.ItemNamed['sessionid'].Value;
end;

// logout stuff
procedure MediaWikiQueryLogoutAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'logout');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryLogoutParseXmlResult(XML: TJclSimpleXML);
begin
  // nothing special to be done
end;

// query, site info general
procedure MediaWikiQuerySiteInfoGeneralAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'general');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoGeneralParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, General: TJclSimpleXMLElem;
  Index: Integer;
  Prop: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  General := Query.Items.ItemNamed['general'];
  Infos.BeginUpdate;
  try
    for Index := 0 to General.Properties.Count - 1 do
    begin
      Prop := General.Properties.Item[Index];
      Infos.Values[Prop.Name] := Prop.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info namespaces
procedure MediaWikiQuerySiteInfoNamespacesAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'namespaces');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoNamespacesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, Namespaces, NameSpace: TJclSimpleXMLElem;
  Index: Integer;
  IDProp, CanonicalProp: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Namespaces := Query.Items.ItemNamed['namespaces'];
  Infos.BeginUpdate;
  try
    for Index := 0 to Namespaces.Items.Count - 1 do
    begin
      NameSpace := Namespaces.Items.Item[Index];
      IDProp := NameSpace.Properties.ItemNamed['id'];
      CanonicalProp := NameSpace.Properties.ItemNamed['canonical'];
      Infos.Values[CanonicalProp.Value] := IDProp.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info, namespace aliases
procedure MediaWikiQuerySiteInfoNamespaceAliasesAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'namespacealiases');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoNamespaceAliasesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, NamespaceAliases, Namespace: TJclSimpleXMLElem;
  Index: Integer;
  Prop: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  NamespaceAliases := Query.Items.ItemNamed['namespacealiases'];
  Infos.BeginUpdate;
  try
    for Index := 0 to NamespaceAliases.Items.Count - 1 do
    begin
      Namespace := NamespaceAliases.Items.Item[Index];
      Prop := Namespace.Properties.ItemNamed['id'];
      Infos.Values[Namespace.Value] := Prop.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info, special page aliases
procedure MediaWikiQuerySiteInfoSpecialPageAliasesAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'specialpagealiases');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoSpecialPageAliasesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, SpecialPageAliases, SpecialPage, Aliases, Alias: TJclSimpleXMLElem;
  I, J: Integer;
  RealName: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  SpecialPageAliases := Query.Items.ItemNamed['specialpagealiases'];
  Infos.BeginUpdate;
  try
    for I := 0 to SpecialPageAliases.Items.Count - 1 do
    begin
      SpecialPage := SpecialPageAliases.Items.Item[I];
      RealName := SpecialPage.Properties.ItemNamed['realname'];
      Aliases := SpecialPage.Items.ItemNamed['aliases'];
      for J := 0 to Aliases.Items.Count - 1 do
      begin
        Alias := Aliases.Items.Item[J];
        Infos.Values[Alias.Value] := RealName.Value;
      end;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info, magic words
procedure MediaWikiQuerySiteInfoMagicWordsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'magicwords');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoMagicWordsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, MagicWords, MagicWord, Aliases, Alias: TJclSimpleXMLElem;
  I, J: Integer;
  NameProp: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  MagicWords := Query.Items.ItemNamed['magicwords'];
  Infos.BeginUpdate;
  try
    for I := 0 to MagicWords.Items.Count - 1 do
    begin
      MagicWord := MagicWords.Items.Item[I];
      NameProp := MagicWord.Properties.ItemNamed['name'];
      Aliases := MagicWord.Items.ItemNamed['aliases'];
      for J := 0 to Aliases.Items.Count - 1 do
      begin
        Alias := Aliases.Items.Item[J];
        Infos.Values[Alias.Value] := NameProp.Value;
      end;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info, statistics
procedure MediaWikiQuerySiteInfoStatisticsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'statistics');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoStatisticsParseXmlResult(XML: TJclSimpleXML; Info: TStrings);
var
  Query, Statistics: TJclSimpleXMLElem;
  Index: Integer;
  Prop: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Statistics := Query.Items.ItemNamed['statistics'];
  Info.BeginUpdate;
  try
    for Index := 0 to Statistics.Properties.Count - 1 do
    begin
      Prop := Statistics.Properties.Item[Index];
      Info.Values[Prop.Name] := Prop.Value;
    end;
  finally
    Info.EndUpdate;
  end;
end;

// query, site info, inter wiki map
procedure MediaWikiQuerySiteInfoInterWikiMapAdd(Queries: TStrings; Local: Boolean; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'interwikimap');
  if Local then
    MediaWikiQueryAdd(Queries, 'sifilteriw', 'local')
  else
    MediaWikiQueryAdd(Queries, 'sifilteriw', '!local');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoInterWikiMapParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, InterWikiMap, InterWiki: TJclSimpleXMLElem;
  Index: Integer;
  PrefixProp, UrlProp: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  InterWikiMap := Query.Items.ItemNamed['interwikimap'];
  Infos.BeginUpdate;
  try
    for Index := 0 to InterWikiMap.Items.Count - 1 do
    begin
      InterWiki := InterWikiMap.Items.Item[Index];
      PrefixProp := InterWiki.Properties.ItemNamed['prefix'];
      UrlProp := InterWiki.Properties.ItemNamed['url'];
      Infos.Values[PrefixProp.Value] := UrlProp.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info , DB replication lag
procedure MediaWikiQuerySiteInfoDBReplLagAdd(Queries: TStrings; ShowAllDB: Boolean;
  OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'dbrepllag');
  if ShowAllDB then
    MediaWikiQueryAdd(Queries, 'sishowalldb');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoDBReplLagParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, DBReplLag, DB: TJclSimpleXMLElem;
  Index: Integer;
  Host, Lag: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  DBReplLag := Query.Items.ItemNamed['dbrepllag'];
  Infos.BeginUpdate;
  try
    for Index := 0 to DBReplLag.Items.Count - 1 do
    begin
      DB := DBReplLag.Items.Item[Index];
      Host := DB.Properties.ItemNamed['host'];
      Lag := DB.Properties.ItemNamed['lag'];
      Infos.Values[Host.Name] := Lag.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info, user groups
procedure MediaWikiQuerySiteInfoUserGroupsAdd(Queries: TStrings; IncludeUserCount: Boolean;
  OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'usergroups');
  if IncludeUserCount then
    MediaWikiQueryAdd(Queries, 'sinumberingroup');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoUserGroupsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, UserGroups, Group, Rights, Permission: TJclSimpleXMLElem;
  I, J: Integer;
  Name: TJclSimpleXMLProp;
  Permissions: string;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserGroups := Query.Items.ItemNamed['usergroups'];
  Infos.BeginUpdate;
  try
    for I := 0 to UserGroups.Items.Count - 1 do
    begin
      Group := UserGroups.Items.Item[I];
      Name := Group.Properties.ItemNamed['name'];
      Rights := Group.Items.ItemNamed['rights'];
      Permissions := '';
      for J := 0 to Rights.Items.Count - 1 do
      begin
        Permission := Rights.Items.Item[J];
        if Permissions <> '' then
          Permissions := Permissions + '|' + Permission.Value
        else
          Permissions := Permission.Value;
      end;
      Infos.Values[Name.Value] := Permissions;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, site info, extensions
procedure MediaWikiQuerySiteInfoExtensionsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'siteinfo');
  MediaWikiQueryAdd(Queries, 'siprop', 'extensions');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQuerySiteInfoExtensionsParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiExtensions);
var
  Query, Extensions, Extension: TJclSimpleXMLElem;
  Index: Integer;
  TypeProp, NameProp, DescriptionProp, DescriptionMsgProp, AuthorProp, VersionProp: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Extensions := Query.Items.ItemNamed['extensions'];
  SetLength(Infos, Extensions.Items.Count);
  for Index := 0 to Extensions.Items.Count - 1 do
  begin
    Extension := Extensions.Items.Item[Index];
    TypeProp := Extension.Properties.ItemNamed['type'];
    NameProp := Extension.Properties.ItemNamed['name'];
    DescriptionProp := Extension.Properties.ItemNamed['description'];
    DescriptionMsgProp := Extension.Properties.ItemNamed['descriptionmsg'];
    AuthorProp := Extension.Properties.ItemNamed['author'];
    VersionProp := Extension.Properties.ItemNamed['version'];
    Infos[Index].ExtensionType := TypeProp.Value;
    Infos[Index].ExtensionName := NameProp.Value;
    Infos[Index].ExtensionDescription := DescriptionProp.Value;
    Infos[Index].ExtensionDescriptionMsg := DescriptionMsgProp.Value;
    Infos[Index].ExtensionAuthor := AuthorProp.Value;
    Infos[Index].ExtensionVersion := VersionProp.Value;
  end;
end;

// query, user info, block info
procedure MediaWikiQueryUserInfoBlockInfoAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'blockinfo');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoBlockInfoParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, BlockInfo: TJclSimpleXMLElem;
  Index: Integer;
  Prop: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  BlockInfo := Query.Items.ItemNamed['userinfo'];
  Infos.BeginUpdate;
  try
    for Index := 0 to BlockInfo.Properties.Count - 1 do
    begin
      Prop := BlockInfo.Properties.Item[Index];
      Infos.Values[Prop.Name] := Prop.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, user info, has msg
procedure MediaWikiQueryUserInfoHasMsgAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'hasmsg');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoHasMsgParseXmlResult(XML: TJclSimpleXML; out HasMessage: Boolean);
var
  Query, UserInfo: TJclSimpleXMLElem;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserInfo := Query.Items.ItemNamed['userinfo'];
  XML.Options := XML.Options - [sxoAutoCreate];
  HasMessage := Assigned(UserInfo.Properties.ItemNamed['messages']);
end;

// query, user info, groups
procedure MediaWikiQueryUserInfoGroupsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'groups');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoGroupsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, UserInfo, Groups, Group: TJclSimpleXMLElem;
  Index: Integer;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserInfo := Query.Items.ItemNamed['userinfo'];
  Groups := UserInfo.Items.ItemNamed['groups'];
  Infos.BeginUpdate;
  try
    for Index := 0 to Groups.Items.Count - 1 do
    begin
      Group := Groups.Items.Item[Index];
      Infos.Add(Group.Value);
    end;
  finally
    Infos.EndUpdate;
  end;
end;

//query, user info, rights
procedure MediaWikiQueryUserInfoRightsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'rights');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoRightsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, UserInfo, Rights, Right: TJclSimpleXMLElem;
  Index: Integer;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserInfo := Query.Items.ItemNamed['userinfo'];
  Rights := UserInfo.Items.ItemNamed['rights'];
  Infos.BeginUpdate;
  try
    for Index := 0 to Rights.Items.Count - 1 do
    begin
      Right := Rights.Items.Item[Index];
      Infos.Add(Right.Value);
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, user info, changeable groups
procedure MediaWikiQueryUserInfoChangeableGroupsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'changeablegroups');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoChangeableGroupsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, UserInfo, ChangeableGroups: TJclSimpleXMLElem;
  Index: Integer;
  Prop: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserInfo := Query.Items.ItemNamed['userinfo'];
  ChangeableGroups := UserInfo.Items.ItemNamed['changeablegroups'];
  Infos.BeginUpdate;
  try
    for Index := 0 to ChangeableGroups.Properties.Count - 1 do
    begin
      Prop := ChangeableGroups.Properties.Item[Index];
      Infos.Values[Prop.Name] := Prop.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, user info, options
procedure MediaWikiQueryUserInfoOptionsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'options');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoOptionsParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, UserInfo, Options: TJclSimpleXMLElem;
  Index: Integer;
  Prop: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserInfo := Query.Items.ItemNamed['userinfo'];
  Options := UserInfo.Items.ItemNamed['options'];
  Infos.BeginUpdate;
  try
    for Index := 0 to Options.Properties.Count - 1 do
    begin
      Prop := Options.Properties.Item[Index];
      Infos.Values[Prop.Name] := Prop.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, user info, edit count
procedure MediaWikiQueryUserInfoEditCountAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'editcount');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoEditCountParseXmlResult(XML: TJclSimpleXML; out EditCount: Integer);
var
  Query, UserInfo: TJclSimpleXMLElem;
  EditCountProp: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserInfo := Query.Items.ItemNamed['userinfo'];
  EditCountProp := UserInfo.Properties.ItemNamed['editcount'];
  EditCount := EditCountProp.IntValue;
end;

// query, user info, rate limits
procedure MediaWikiQueryUserInfoRateLimitsAdd(Queries: TStrings; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'userinfo');
  MediaWikiQueryAdd(Queries, 'uiprop', 'ratelimits');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryUserInfoRateLimitsParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiRateLimits);
var
  Query, UserInfo, RateLimits, RateLimit, Group: TJclSimpleXMLElem;
  I, J, K: Integer;
  HitsProp, SecondsProp: TJclSimpleXMLProp;
begin
  SetLength(Infos, 0);
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  UserInfo := Query.Items.ItemNamed['userinfo'];
  RateLimits := UserInfo.Items.ItemNamed['ratelimits'];
  for I := 0 to RateLimits.Items.Count - 1 do
  begin
    RateLimit := RateLimits.Items.Item[I];

    for J := 0 to RateLimit.Items.Count - 1 do
    begin
      Group := RateLimit.Items.Item[J];
      HitsProp := Group.Properties.ItemNamed['hits'];
      SecondsProp := Group.Properties.ItemNamed['seconds'];

      K := Length(Infos);
      SetLength(Infos, K + 1);
      Infos[K].RateLimitAction := RateLimit.Name;
      Infos[K].RateLimitGroup := Group.Name;
      Infos[K].RateLimitHits := HitsProp.IntValue;
      Infos[K].RateLimitSeconds := SecondsProp.IntValue;
    end;
  end;
end;

// query, messages
procedure MediaWikiQueryMessagesAdd(Queries: TStrings; const NameFilter, ContentFilter, Lang: string;
  OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'meta', 'allmessages');
  if NameFilter <> '' then
    MediaWikiQueryAdd(Queries, 'ammessages', NameFilter);
  if ContentFilter <> '' then
    MediaWikiQueryAdd(Queries, 'amfilter', ContentFilter);
  if Lang <> '' then
    MediaWikiQueryAdd(Queries, 'amlang', Lang);
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryMessagesParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, AllMessages, Message: TJclSimpleXMLElem;
  Index: Integer;
  Name: TJclSimpleXMLProp;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  AllMessages := Query.Items.ItemNamed['allmessages'];
  Infos.BeginUpdate;
  try
    for Index := 0 to AllMessages.Items.Count - 1 do
    begin
      Message := AllMessages.Items.Item[Index];
      Name := Message.Properties.ItemNamed['name'];
      Infos.Values[Name.Value] := Message.Value;
    end;
  finally
    Infos.EndUpdate;
  end;
end;

// query, page info
procedure MediaWikiQueryPageInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean; Flags: TMediaWikiPageInfoFlags;
  OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'prop', 'info');
  if PageID then
    MediaWikiQueryAdd(Queries, 'pageids', Titles)
  else
    MediaWikiQueryAdd(Queries, 'titles', Titles);
  if mwfIncludeProtection in Flags then
    MediaWikiQueryAdd(Queries, 'inprop', 'protection');
  if mwfIncludeTalkID in Flags then
    MediaWikiQueryAdd(Queries, 'inprop', 'talkid');
  if mwfIncludeSubjectID in Flags then
    MediaWikiQueryAdd(Queries, 'inprop', 'subjectid');
  if mwfIncludeURL in Flags then
    MediaWikiQueryAdd(Queries, 'inprop', 'url');
  if mwfIncludeEditToken in Flags then
    MediaWikiQueryAdd(Queries, 'intoken', 'edit');
  if mwfIncludeMoveToken in Flags then
    MediaWikiQueryAdd(Queries, 'intoken', 'move');
  if mwfIncludeDeleteToken in Flags then
    MediaWikiQueryAdd(Queries, 'intoken', 'delete');
  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryPageInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageInfos);
var
  Query, Pages, Page, Protections, Protection: TJclSimpleXMLElem;
  I, J: Integer;
  ID, Namespace, Title, LastTouched, RevID, Views, Size, Redirect, New,
  TalkID, SubjectID, FullURL, EditURL, TypeProp, Level, Expiry,
  EditToken, MoveToken, DeleteToken: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Pages := Query.Items.ItemNamed['pages'];
  SetLength(Infos, Pages.Items.Count);
  for I := 0 to Pages.Items.Count - 1 do
  begin
    Page := Pages.Items.Item[I];
    ID := Page.Properties.ItemNamed['pageid'];
    Namespace := Page.Properties.ItemNamed['ns'];
    Title := Page.Properties.ItemNamed['title'];
    LastTouched := Page.Properties.ItemNamed['touched'];
    RevID := Page.Properties.ItemNamed['lastrevid'];
    Views := Page.Properties.ItemNamed['counter'];
    Size := Page.Properties.ItemNamed['length'];
    XML.Options := XML.Options - [sxoAutoCreate];
    Redirect := Page.Properties.ItemNamed['redirect'];
    New := Page.Properties.ItemNamed['new'];
    Protections := Page.Items.ItemNamed['protection'];
    TalkID := Page.Properties.ItemNamed['talkid'];
    SubjectID := Page.Properties.ItemNamed['subjectid'];
    FullURL := Page.Properties.ItemNamed['fullurl'];
    EditURL := Page.Properties.ItemNamed['editurl'];
    EditToken := Page.Properties.ItemNamed['edittoken'];
    MoveToken := Page.Properties.ItemNamed['movetoken'];
    DeleteToken := Page.Properties.ItemNamed['deletetoken'];

    XML.Options := XML.Options + [sxoAutoCreate];
    Infos[I].PageBasics.PageID := ID.IntValue;
    Infos[I].PageBasics.PageNamespace := Namespace.IntValue;
    Infos[I].PageBasics.PageTitle := Title.Value;
    Infos[I].PageLastTouched := StrISO8601ToDateTime(LastTouched.Value);
    Infos[I].PageRevisionID := RevID.IntValue;
    Infos[I].PageViews := Views.IntValue;
    Infos[I].PageSize := Size.IntValue;
    if Assigned(Redirect) then
      Include(Infos[I].PageFlags, mwfPageIsRedirect);
    if Assigned(New) then
      Include(Infos[I].PageFlags, mwfPageIsNew);
    if Assigned(Protections) then
    begin
      SetLength(Infos[I].PageProtections, Protections.Items.Count);
      for J := 0 to Protections.Items.Count - 1 do
      begin
        Protection := Protections.Items.Item[J];
        TypeProp := Protection.Properties.ItemNamed['type'];
        Level := Protection.Properties.ItemNamed['level'];
        Expiry := Protection.Properties.ItemNamed['expiry'];
        Infos[I].PageProtections[J].PageProtectionAction := TypeProp.Value;
        Infos[I].PageProtections[J].PageProtectionGroup := Level.Value;
        Infos[I].PageProtections[J].PageProtectionExpiry := Expiry.Value;
      end;
    end;
    if Assigned(TalkID) then
      Infos[I].PageTalkID := TalkID.IntValue;
    if Assigned(SubjectID) then
      Infos[I].PageSubjectID := SubjectID.IntValue;
    if Assigned(FullURL) then
      Infos[I].PageFullURL := FullURL.Value;
    if Assigned(EditURL) then
      Infos[I].PageEditURL := EditURL.Value;
    if Assigned(EditToken) then
      Infos[I].PageEditToken := EditToken.Value;
    if Assigned(MoveToken) then
      Infos[I].PageMoveToken := MoveToken.Value;
    if Assigned(DeleteToken) then
      Infos[I].PageDeleteToken := DeleteToken.Value;
  end;
end;

// query, page info, revision info
procedure MediaWikiQueryPageRevisionInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  Flags: TMediaWikiPageRevisionInfoFlags; MaxRevisions, Section: Integer; StartRevisionID, EndRevisionID: TMediaWikiID;
  const StartDateTime, EndDateTime: TDateTime; const IncludeUser, ExcludeUser: string; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'prop', 'revisions');
  if PageID then
    MediaWikiQueryAdd(Queries, 'pageids', Titles)
  else
    MediaWikiQueryAdd(Queries, 'titles', Titles);
  if mwfIncludeRevisionID in Flags then
    MediaWikiQueryAdd(Queries, 'rvprop', 'ids');
  if mwfIncludeRevisionFlags in Flags then
    MediaWikiQueryAdd(Queries, 'rvprop', 'flags');
  if mwfIncludeRevisionTimeStamp in Flags then
    MediaWikiQueryAdd(Queries, 'rvprop', 'timestamp');
  if mwfIncludeRevisionAuthor in Flags then
    MediaWikiQueryAdd(Queries, 'rvprop', 'user');
  if mwfIncludeRevisionComment in Flags then
    MediaWikiQueryAdd(Queries, 'rvprop', 'comment');
  if mwfIncludeRevisionSize in Flags then
    MediaWikiQueryAdd(Queries, 'rvprop', 'size');
  if mwfIncludeRevisionContent in Flags then
    MediaWikiQueryAdd(Queries, 'rvprop', 'content');
  //if mwfTags in Flags then
  //  MediaWikiQueryAdd(Queries, 'rvprop', 'tags');

  if mwfRevisionReverseOrder in Flags then
    MediaWikiQueryAdd(Queries, 'rvdir', 'newer')
  else
    MediaWikiQueryAdd(Queries, 'rvdir', 'older');

  if mwfIncludeRevisionRollbackToken in Flags then
    MediaWikiQueryAdd(Queries, 'rvtoken', 'rollback');

  if mwfRevisionContentXml in Flags then
    MediaWikiQueryAdd(Queries, 'rvgeneratexml');

  if mwfRevisionContentExpandTemplates in Flags then
    MediaWikiQueryAdd(Queries, 'rvexpandtemplates');

  if mwfRevisionContinue in Flags then
    MediaWikiQueryAdd(Queries, 'rvcontinue');

  if MaxRevisions > 0 then
    MediaWikiQueryAdd(Queries, 'rvlimit', IntToStr(MaxRevisions));

  if Section >= 0 then
    MediaWikiQueryAdd(Queries, 'rvsection', IntToStr(Section));

  if StartRevisionID >= 0 then
    MediaWikiQueryAdd(Queries, 'rvstartid', IntToStr(StartRevisionID));

  if EndRevisionID >= 0 then
    MediaWikiQueryAdd(Queries, 'rvendid', IntToStr(EndRevisionID));

  if (StartRevisionID < 0) and (StartDateTime <> 0.0) then
    MediaWikiQueryAdd(Queries, 'rvstart', DateTimeToStrISO8601(StartDateTime), True);

  if (EndRevisionID < 0) and (EndDateTime <> 0.0) then
    MediaWikiQueryAdd(Queries, 'rvend', DateTimeToStrISO8601(EndDateTime), True);

  if IncludeUser <> '' then
    MediaWikiQueryAdd(Queries, 'rvuser', IncludeUser);

  if ExcludeUser <> '' then
    MediaWikiQueryAdd(Queries, 'rvexcludeuser', ExcludeUser);

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryPageRevisionInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageRevisionInfos);
var
  Query, Pages, Page, Revisions, Revision: TJclSimpleXMLElem;
  I, J, K: Integer;
  PageID, NameSpace, Title, RevID, Minor, Author, TimeStamp, Size, Comment, RollbackToken: TJclSimpleXMLProp;
begin
  SetLength(Infos, 0);
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Pages := Query.Items.ItemNamed['pages'];
  for I := 0 to Pages.Items.Count - 1 do
  begin
    Page := Pages.Items.Item[I];
    PageID := Page.Properties.ItemNamed['pageid'];
    NameSpace := Page.Properties.ItemNamed['ns'];
    Title := Page.Properties.ItemNamed['title'];

    Revisions := Page.Items.ItemNamed['revisions'];
    for J := 0 to Revisions.Items.Count - 1 do
    begin
      Revision := Revisions.Items.Item[J];

      RevID := Revision.Properties.ItemNamed['revid'];
      Author := Revision.Properties.ItemNamed['user'];
      TimeStamp := Revision.Properties.ItemNamed['timestamp'];
      Size := Revision.Properties.ItemNamed['size'];
      Comment := Revision.Properties.ItemNamed['comment'];
      RollbackToken := Revision.Properties.ItemNamed['rollbacktoken'];

      XML.Options := XML.Options - [sxoAutoCreate];
      Minor := Revision.Properties.ItemNamed['minor'];
      XML.Options := XML.Options + [sxoAutoCreate];

      K := Length(Infos);
      SetLength(Infos, K + 1);
      Infos[K].PageRevisionInfoPageBasics.PageTitle := Title.Value;
      Infos[K].PageRevisionInfoPageBasics.PageNamespace := NameSpace.IntValue;
      Infos[K].PageRevisionInfoPageBasics.PageID := PageID.IntValue;
      Infos[K].PageRevisionInfoID := RevID.IntValue;
      if Assigned(Minor) then
        Include(Infos[K].PageRevisionInfoFlags, mwfMinorEdit);
      Infos[K].PageRevisionInfoDateTime := StrISO8601ToDateTime(TimeStamp.Value);
      Infos[K].PageRevisionInfoAuthor := Author.Value;
      Infos[K].PageRevisionInfoComment := Comment.Value;
      Infos[K].PageRevisionInfoSize := Size.IntValue;
      Infos[K].PageRevisionInfoContent := Revision.Value;
      Infos[K].PageRevisionInfoRollbackToken := RollbackToken.Value;
    end;
  end;
end;


procedure MediaWikiQueryPageRevisionInfoParseXmlResult(XML: TJclSimpleXML; out StartID, EndID: TMediaWikiID);
var
  QueryContinue, Revisions: TJclSimpleXMLElem;
  RevStartID, RevEndID: TJclSimpleXMLProp;
begin
  StartID := -1;
  EndID := -1;
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  Revisions := QueryContinue.Items.ItemNamed['revisions'];
  if not Assigned(Revisions) then
    Exit;
  RevStartID := Revisions.Properties.ItemNamed['rvstartid'];
  RevEndID := Revisions.Properties.ItemNamed['rvendid'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(RevStartID) then
    StartID := RevStartID.IntValue;
  if Assigned(RevEndID) then
    EndID := RevEndID.IntValue;
end;

// query, page info, category info
procedure MediaWikiQueryPageCategoryInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  Flags: TMediaWikiPageCategoryInfoFlags; MaxCategories: Integer;
  const CategoryTitles, CategoryStart: string; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'prop', 'categories');
  if PageID then
    MediaWikiQueryAdd(Queries, 'pageids', Titles)
  else
    MediaWikiQueryAdd(Queries, 'titles', Titles);

  if mwfIncludeCategorySortKey in Flags then
    MediaWikiQueryAdd(Queries, 'clprop', 'sortkey');
  if mwfIncludeCategoryTimeStamp in Flags then
    MediaWikiQueryAdd(Queries, 'clprop', 'timestamp');
  if mwfCategoryHidden in Flags then
    MediaWikiQueryAdd(Queries, 'clshow', 'hidden')
  else
    MediaWikiQueryAdd(Queries, 'clshow', '!hidden');

  if MaxCategories > 0 then
    MediaWikiQueryAdd(Queries, 'cllimit', IntToStr(MaxCategories));

  if CategoryTitles <> '' then
    MediaWikiQueryAdd(Queries, 'clcategories', CategoryTitles);

  if CategoryStart <> '' then
    MediaWikiQueryAdd(Queries, 'clcontinue', CategoryStart);

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryPageCategoryInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageCategoryInfos);
var
  Query, Pages, Page, Categories, Category: TJclSimpleXMLElem;
  I, J, K: Integer;
  PageID, PageNamespace, PageTitle, Namespace, Title, SortKey, TimeStamp: TJclSimpleXMLProp;
begin
  SetLength(Infos, 0);
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Pages := Query.Items.ItemNamed['pages'];
  for I := 0 to Pages.Items.Count - 1 do
  begin
    Page := Pages.Items.Item[I];

    PageID := Page.Properties.ItemNamed['pageid'];
    PageNamespace := Page.Properties.ItemNamed['ns'];
    PageTitle := Page.Properties.ItemNamed['title'];

    Categories := Page.Items.ItemNamed['categories'];
    for J := 0 to Categories.Items.Count - 1 do
    begin
      Category := Categories.Items.Item[J];

      Namespace := Category.Properties.ItemNamed['ns'];
      Title := Category.Properties.ItemNamed['title'];
      SortKey := Category.Properties.ItemNamed['sortkey'];
      TimeStamp := Category.Properties.ItemNamed['timestamp'];

      K := Length(Infos);
      SetLength(Infos, K + 1);
      Infos[K].CategoryPageBasics.PageTitle := PageTitle.Value;
      Infos[K].CategoryPageBasics.PageNamespace := PageNamespace.IntValue;
      Infos[K].CategoryPageBasics.PageID := PageID.IntValue;
      Infos[K].CategoryTitle := Title.Value;
      Infos[K].CategoryNameSpace := Namespace.IntValue;
      Infos[K].CategoryTimeStamp := StrISO8601ToDateTime(TimeStamp.Value);
      Infos[K].CategorySortKey := SortKey.Value;
    end;
  end;
end;

procedure MediaWikiQueryPageCategoryInfoParseXmlResult(XML: TJclSimpleXML; out StartCategory: string);
var
  QueryContinue, Categories: TJclSimpleXMLElem;
  clContinue: TJclSimpleXMLProp;
begin
  StartCategory := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  Categories := QueryContinue.Items.ItemNamed['categories'];
  if not Assigned(Categories) then
    Exit;
  clContinue := Categories.Properties.ItemNamed['clcontinue'];

  if Assigned(clContinue) then
    StartCategory := clContinue.Value;
end;

// query, page info, link info
procedure MediaWikiQueryPageLinkInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  MaxLinks, Namespace: Integer; const LinkStart: string; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'prop', 'links');

  if PageID then
    MediaWikiQueryAdd(Queries, 'pageids', Titles)
  else
    MediaWikiQueryAdd(Queries, 'titles', Titles);

  if MaxLinks > 0 then
    MediaWikiQueryAdd(Queries, 'pllimit', IntToStr(MaxLinks));

  if Namespace >= 0 then
    MediaWikiQueryAdd(Queries, 'plnamespace', IntToStr(Namespace));

  if LinkStart <> '' then
    MediaWikiQueryAdd(Queries, 'plcontinue', LinkStart);

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryPageLinkInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageLinkInfos);
var
  Query, Pages, Page, Links, Link: TJclSimpleXMLElem;
  I, J, K: Integer;
  PageID, PageTitle, PageNamespace, TargetNamespace, TargetTitle: TJclSimpleXMLProp;
begin
  SetLength(Infos, 0);
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Pages := Query.Items.ItemNamed['pages'];
  for I := 0 to Pages.Items.Count - 1 do
  begin
    Page := Pages.Items.Item[I];

    PageID := Page.Properties.ItemNamed['pageid'];
    PageTitle := Page.Properties.ItemNamed['title'];
    PageNamespace := Page.Properties.ItemNamed['ns'];

    Links := Page.Items.ItemNamed['links'];

    for J := 0 to Links.Items.Count - 1 do
    begin
      Link := Links.Items.Item[J];

      TargetNamespace := Link.Properties.ItemNamed['ns'];
      TargetTitle := Link.Properties.ItemNamed['title'];

      K := Length(Infos);
      SetLength(Infos, K + 1);
      Infos[K].LinkSourceBasics.PageTitle := PageTitle.Value;
      Infos[K].LinkSourceBasics.PageNamespace := PageNamespace.IntValue;
      Infos[K].LinkSourceBasics.PageID := PageID.IntValue;
      Infos[K].LinkTargetTitle := TargetTitle.Value;
      Infos[K].LinkTargetNameSpace := TargetNamespace.IntValue;
    end;
  end;
end;

procedure MediaWikiQueryPageLinkInfoParseXmlResult(XML: TJclSimpleXML; out StartCategory: string);
var
  QueryContinue, Links: TJclSimpleXMLElem;
  plContinue: TJclSimpleXMLProp;
begin
  StartCategory := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  Links := QueryContinue.Items.ItemNamed['links'];
  if not Assigned(Links) then
    Exit;
  plContinue := Links.Properties.ItemNamed['plcontinue'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(plContinue) then
    StartCategory := plContinue.Value;
end;

// query, page info, template info
procedure MediaWikiQueryPageTemplateInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  MaxTemplates, Namespace: Integer; TemplateStart: string; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'prop', 'templates');

  if PageID then
    MediaWikiQueryAdd(Queries, 'pageids', Titles)
  else
    MediaWikiQueryAdd(Queries, 'titles', Titles);

  if MaxTemplates > 0 then
    MediaWikiQueryAdd(Queries, 'tllimit', IntToStr(MaxTemplates));

  if Namespace >= 0 then
    MediaWikiQueryAdd(Queries, 'tlnamespace', IntToStr(Namespace));

  if TemplateStart <> '' then
    MediaWikiQueryAdd(Queries, 'tlcontinue', TemplateStart);

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryPageTemplateInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageTemplateInfos);
var
  Query, Pages, Page, Templates, Template: TJclSimpleXMLElem;
  I, J, K: Integer;
  PageTitle, PageID, PageNamespace, TemplateNamespace, TemplateTitle: TJclSimpleXMLProp;
begin
  SetLength(Infos, 0);
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Pages := Query.Items.ItemNamed['pages'];
  for I := 0 to Pages.Items.Count - 1 do
  begin
    Page := Pages.Items.Item[I];

    PageTitle := Page.Properties.ItemNamed['title'];
    PageID := Page.Properties.ItemNamed['pageid'];
    PageNamespace := Page.Properties.ItemNamed['ns'];

    Templates := Page.Items.ItemNamed['templates'];
    for J := 0 to Templates.Items.Count - 1 do
    begin
      Template := Templates.Items.Item[J];

      TemplateNamespace := Template.Properties.ItemNamed['ns'];
      TemplateTitle := Template.Properties.ItemNamed['title'];

      K := Length(Infos);
      SetLength(Infos, K + 1);
      Infos[K].TemplatePageBasics.PageTitle := PageTitle.Value;
      Infos[K].TemplatePageBasics.PageNamespace := PageNamespace.IntValue;
      Infos[K].TemplatePageBasics.PageID := PageID.IntValue;
      Infos[K].TemplateTitle := TemplateTitle.Value;
      Infos[K].TemplateNameSpace := TemplateNamespace.IntValue;
    end;
  end;
end;

procedure MediaWikiQueryPageTemplateInfoParseXmlResult(XML: TJclSimpleXML; out StartTemplate: string);
var
  QueryContinue, Templates: TJclSimpleXMLElem;
  tlContinue: TJclSimpleXMLProp;
begin
  StartTemplate := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  Templates := QueryContinue.Items.ItemNamed['templates'];
  if not Assigned(Templates) then
    Exit;
  tlContinue := Templates.Properties.ItemNamed['tlcontinue'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(tlContinue) then
    StartTemplate :=tlContinue.Value;
end;

// query, page info, ext links
procedure MediaWikiQueryPageExtLinkInfoAdd(Queries: TStrings; const Titles: string; PageID: Boolean;
  MaxLinks: Integer; const StartLink: string; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'prop', 'extlinks');

  if PageID then
    MediaWikiQueryAdd(Queries, 'pageids', Titles)
  else
    MediaWikiQueryAdd(Queries, 'titles', Titles);

  if MaxLinks > 0 then
    MediaWikiQueryAdd(Queries, 'ellimit', IntToStr(MaxLinks));

  if StartLink <> '' then
    MediaWikiQueryAdd(Queries, 'eloffset', StartLink, True);

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryPageExtLinkInfoParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiPageExtLinkInfos);
var
  Query, Pages, Page, ExtLinks, ExtLink: TJclSimpleXMLElem;
  I, J, K: Integer;
  PageTitle, PageID, PageNamespace: TJclSimpleXMLProp;
begin
  SetLength(Infos, 0);
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Pages := Query.Items.ItemNamed['pages'];
  for I := 0 to Pages.Items.Count - 1 do
  begin
    Page := Pages.Items.Item[I];

    PageTitle := Page.Properties.ItemNamed['title'];
    PageID := Page.Properties.ItemNamed['pageid'];
    PageNamespace := Page.Properties.ItemNamed['ns'];

    ExtLinks := Page.Items.ItemNamed['extlinks'];
    for J := 0 to ExtLinks.Items.Count - 1 do
    begin
      ExtLink := ExtLinks.Items.Item[J];

      K := Length(Infos);
      SetLength(Infos, K + 1);
      Infos[K].ExtLinkPageBasics.PageTitle := PageTitle.Value;
      Infos[K].ExtLinkPageBasics.PageID := PageID.IntValue;
      Infos[K].ExtLinkPageBasics.PageNamespace := PageNamespace.IntValue;
      Infos[K].ExtLinkTarget := ExtLink.Value;
    end;
  end;
end;

procedure MediaWikiQueryPageExtLinkInfoParseXmlResult(XML: TJclSimpleXML; out StartExtLink: string);
var
  QueryContinue, ExtLinks: TJclSimpleXMLElem;
  elOffset: TJclSimpleXMLProp;
begin
  StartExtLink := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  ExtLinks := QueryContinue.Items.ItemNamed['extlinks'];
  if not Assigned(ExtLinks) then
    Exit;
  elOffset := ExtLinks.Properties.ItemNamed['eloffset'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(elOffset) then
    StartExtLink := elOffset.Value;
end;

// query, list, all pages
procedure MediaWikiQueryAllPageAdd(Queries: TStrings; const StartPage, Prefix: string; MaxPage: Integer;
  Namespace: Integer; RedirFilter: TMediaWikiAllPageFilterRedir; LangFilter: TMediaWikiAllPageFilterLang;
  MinSize, MaxSize: Integer; ProtectionFilter: TMediaWikiAllPageFilterProtection;
  LevelFilter: TMediaWikiAllPageFilterLevel; Direction: TMediaWikiAllPageDirection; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'list', 'allpages');

  if StartPage <> '' then
    MediaWikiQueryAdd(Queries, 'apfrom', StartPage, True);

  if Prefix <> '' then
    MediaWikiQueryAdd(Queries, 'apprefix', Prefix);

  if MaxPage > 0 then
    MediaWikiQueryAdd(Queries, 'aplimit', IntToStr(MaxPage));

  if Namespace >= 0 then
    MediaWikiQueryAdd(Queries, 'apnamespace', IntToStr(Namespace));

  if RedirFilter = mwfAllPageFilterRedirect then
    MediaWikiQueryAdd(Queries, 'apfilterredir', 'redirects')
  else
  if RedirFilter = mwfAllPageFilterNonRedirect then
    MediaWikiQueryAdd(Queries, 'apfilterredir', 'nonredirects');

  if LangFilter = mwfAllPageLangOnly then
    MediaWikiQueryAdd(Queries, 'apfilterlanglinks', 'withlanglinks')
  else
  if LangFilter = mwfAllPageLangNone then
    MediaWikiQueryAdd(Queries, 'apfilterlanglinks', 'withoutlanglinks');

  if MinSize >= 0 then
    MediaWikiQueryAdd(Queries, 'apminsize', IntToStr(MinSize));

  if MaxSize >= 0 then
    MediaWikiQueryAdd(Queries, 'apmaxsize', IntToStr(MaxSize));

  if ProtectionFilter = mwfAllPageProtectionEdit then
    MediaWikiQueryAdd(Queries, 'apprtype', 'edit')
  else
  if ProtectionFilter = mwfAllPageProtectionMove then
    MediaWikiQueryAdd(Queries, 'apprtype', 'move');

  if LevelFilter = mwfAllPageLevelAutoConfirmed then
    MediaWikiQueryAdd(Queries, 'apprlevel', 'autoconfirmed')
  else
  if LevelFilter = mwfAllPageLevelSysops then
    MediaWikiQueryAdd(Queries, 'apprlevel', 'sysop');

  if Direction = mwfAllPageAscending then
    MediaWikiQueryAdd(Queries, 'apdir', 'ascending')
  else
    MediaWikiQueryAdd(Queries, 'apdir', 'descending');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryAllPageParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiAllPageInfos);
var
  Query, AllPages, Page: TJclSimpleXMLElem;
  Index: Integer;
  PageTitle, PageID, PageNamespace: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  AllPages := Query.Items.ItemNamed['allpages'];
  SetLength(Infos, AllPages.Items.Count);
  for Index := 0 to AllPages.Items.Count - 1 do
  begin
    Page := AllPages.Items.Item[Index];

    PageTitle := Page.Properties.ItemNamed['title'];
    PageID := Page.Properties.ItemNamed['pageid'];
    PageNamespace := Page.Properties.ItemNamed['ns'];

    Infos[Index].PageTitle := PageTitle.Value;
    Infos[Index].PageID := PageID.IntValue;
    Infos[Index].PageNamespace := PageNamespace.IntValue;
  end;
end;

procedure MediaWikiQueryAllPageParseXmlResult(XML: TJclSimpleXML; out StartPage: string);
var
  QueryContinue, AllPages: TJclSimpleXMLElem;
  apFrom: TJclSimpleXMLProp;
begin
  StartPage := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  AllPages := QueryContinue.Items.ItemNamed['allpages'];
  if not Assigned(AllPages) then
    Exit;
  apFrom := AllPages.Properties.ItemNamed['apfrom'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(apFrom) then
    StartPage := apFrom.Value;
end;

procedure MediaWikiQueryAllLinkAdd(Queries: TStrings; const StartLink, Prefix: string; MaxLink: Integer;
  const ContinueLink: string; Namespace: Integer; Flags: TMediaWikiAllLinkInfoFlags;
  OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'list', 'alllinks');

  if StartLink <> '' then
    MediaWikiQueryAdd(Queries, 'alfrom', StartLink);

  if ContinueLink <> '' then
    MediaWikiQueryAdd(Queries, 'alcontinue', ContinueLink, True);

  if Prefix <> '' then
    MediaWikiQueryAdd(Queries, 'alprefix', Prefix);

  if MaxLink > 0 then
    MediaWikiQueryAdd(Queries, 'allimit', IntToStr(MaxLink));

  if Namespace >= 0 then
    MediaWikiQueryAdd(Queries, 'alnamespace', IntToStr(Namespace));

  if mwfLinkUnique in Flags then
    MediaWikiQueryAdd(Queries, 'alunique')
  else
  if mwfLinkIncludePageID in Flags then
    MediaWikiQueryAdd(Queries, 'alprop', 'ids');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryAllLinkParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiAllLinkInfos);
var
  Query, AllLinks, Link: TJclSimpleXMLElem;
  Index: Integer;
  LinkTitle, PageID, LinkNamespace: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  AllLinks := Query.Items.ItemNamed['alllinks'];
  SetLength(Infos, AllLinks.Items.Count);
  for Index := 0 to AllLinks.Items.Count - 1 do
  begin
    Link := AllLinks.Items.Item[Index];

    LinkTitle := Link.Properties.ItemNamed['title'];
    PageID := Link.Properties.ItemNamed['fromid'];
    LinkNamespace := Link.Properties.ItemNamed['ns'];

    Infos[Index].LinkTitle := LinkTitle.Value;
    Infos[Index].PageID := PageID.IntValue;
    Infos[Index].LinkNamespace := LinkNamespace.IntValue;
  end;
end;

procedure MediaWikiQueryAllLinkParseXmlResult(XML: TJclSimpleXML; out StartPage: string);
var
  QueryContinue, AllLinks: TJclSimpleXMLElem;
  alFrom: TJclSimpleXMLProp;
begin
  StartPage := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  AllLinks := QueryContinue.Items.ItemNamed['alllinks'];
  if not Assigned(AllLinks) then
    Exit;
  alFrom := AllLinks.Properties.ItemNamed['alcontinue'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(alFrom) then
    StartPage := alFrom.Value;
end;

procedure MediaWikiQueryAllCategoryAdd(Queries: TStrings; const StartCategory, Prefix: string; MaxCategory: Integer;
  Flags: TMediaWikiAllCategoryInfoFlags; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'list', 'allcategories');

  if StartCategory <> '' then
    MediaWikiQueryAdd(Queries, 'acfrom', StartCategory, True);

  if Prefix <> '' then
    MediaWikiQueryAdd(Queries, 'acprefix', Prefix);

  if MaxCategory > 0 then
    MediaWikiQueryAdd(Queries, 'aclimit', IntToStr(MaxCategory));

  if mwfCategoryDescending in Flags then
    MediaWikiQueryAdd(Queries, 'acdir', 'descending');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryAllCategoryParseXmlResult(XML: TJclSimpleXML; Infos: TStrings);
var
  Query, AllCategories, Category: TJclSimpleXMLElem;
  Index: Integer;
begin
  Infos.Clear;
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  AllCategories := Query.Items.ItemNamed['allcategories'];
  Infos.BeginUpdate;
  try
    for Index := 0 to AllCategories.Items.Count - 1 do
    begin
      Category := AllCategories.Items.Item[Index];
      Infos.Add(Category.Value);
    end;
  finally
    Infos.EndUpdate
  end;
end;

procedure MediaWikiQueryAllCategoryParseXmlResult(XML: TJclSimpleXML; out StartCategory: string);
var
  QueryContinue, AllCategories: TJclSimpleXMLElem;
  acFrom: TJclSimpleXMLProp;
begin
  StartCategory := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  AllCategories := QueryContinue.Items.ItemNamed['allcategories'];
  if not Assigned(AllCategories) then
    Exit;
  acFrom := AllCategories.Properties.ItemNamed['acfrom'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(acFrom) then
    StartCategory := acFrom.Value;
end;

procedure MediaWikiQueryAllUserAdd(Queries: TStrings; const StartUser, Prefix, Group: string; MaxUser: Integer;
  Flags: TMediaWikiAllUserInfoFlags; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'list', 'allusers');

  if StartUser <> '' then
    MediaWikiQueryAdd(Queries, 'aufrom', StartUser);

  if Prefix <> '' then
    MediaWikiQueryAdd(Queries, 'auprefix', Prefix);

  if Group <> '' then
    MediaWikiQueryAdd(Queries, 'augroup', Group);

  if MaxUser > 0 then
    MediaWikiQueryAdd(Queries, 'aclimit', IntToStr(MaxUser));

  if mwfIncludeUserEditCount in Flags then
    MediaWikiQueryAdd(Queries, 'auprop', 'editcount');
  if mwfIncludeUserGroups in Flags then
    MediaWikiQueryAdd(Queries, 'auprop', 'groups');
  if mwfIncludeUserRegistration in Flags then
    MediaWikiQueryAdd(Queries, 'auprop', 'registration');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryAllUserParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiAllUserInfos);
var
  Query, AllUsers, User, Groups, Group: TJclSimpleXMLElem;
  I, J: Integer;
  Name, EditCount, Registration: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  AllUsers := Query.Items.ItemNamed['allusers'];
  SetLength(Infos, AllUsers.Items.Count);
  for I := 0 to AllUsers.Items.Count - 1 do
  begin
    User := AllUsers.Items.Item[I];

    Name := User.Properties.ItemNamed['name'];
    EditCount := User.Properties.ItemNamed['editcount'];
    Registration := User.Properties.ItemNamed['registration'];

    Infos[I].UserName := Name.Value;
    Infos[I].UserEditCount := EditCount.IntValue;
    Infos[I].UserRegistration := StrISO8601ToDateTime(Registration.Value);

    Groups := User.Items.ItemNamed['groups'];
    SetLength(Infos[I].UserGroups, Groups.Items.Count);
    for J := 0 to Groups.Items.Count - 1 do
    begin
      Group := Groups.Items.Item[J];
      Infos[I].UserGroups[J] := Group.Value;
    end;
  end;
end;

procedure MediaWikiQueryAllUserParseXmlResult(XML: TJclSimpleXML; out StartUser: string);
var
  QueryContinue, AllUsers: TJclSimpleXMLElem;
  auFrom: TJclSimpleXMLProp;
begin
  StartUser := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  AllUsers := QueryContinue.Items.ItemNamed['allusers'];
  if not Assigned(AllUsers) then
    Exit;
  auFrom := AllUsers.Properties.ItemNamed['aufrom'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(auFrom) then
    StartUser := auFrom.Value;
end;

procedure MediaWikiQueryBackLinkAdd(Queries: TStrings; const BackLinkTitle: string; Namespace, MaxLink: Integer; const StartBackLink: string;
  Flags: TMediaWikiBackLinkInfoFlags; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'list', 'backlinks');

  if BackLinkTitle <> '' then
    MediaWikiQueryAdd(Queries, 'bltitle', BackLinkTitle);

  if NameSpace >= 0 then
    MediaWikiQueryAdd(Queries, 'blnamespace', IntToStr(Namespace));

  if MaxLink > 0 then
    MediaWikiQueryAdd(Queries, 'bllimit', IntToStr(MaxLink));

  if StartBackLink <> '' then
    MediaWikiQueryAdd(Queries, 'blcontinue', StartBackLink, True);

  if mwfIncludeBackLinksFromRedirect in Flags then
    MediaWikiQueryAdd(Queries, 'blredirect');

  if mwfExcludeBackLinkRedirect in Flags then
    MediaWikiQueryAdd(Queries, 'blfilterredir', 'nonredirects')
  else
  if mwfExcludeBackLinkNonRedirect in Flags then
    MediaWikiQueryAdd(Queries, 'blfilterredir', 'redirects')
  else
    MediaWikiQueryAdd(Queries, 'blfilterredir', 'all');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryBackLinkParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiBackLinkInfos);
var
  Query, BackLinks, BackLink, RedirLinks, RedirLink: TJclSimpleXMLElem;
  I, J, K: Integer;
  PageID, PageNamespace, PageTitle, Redirect, RedirPageID, RedirPageNamespace, RedirPageTitle: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  BackLinks := Query.Items.ItemNamed['backlinks'];
  SetLength(Infos, 0);
  for I := 0 to BackLinks.Items.Count - 1 do
  begin
    BackLink := BackLinks.Items.Item[I];

    PageID := BackLink.Properties.ItemNamed['pageid'];
    PageNamespace := BackLink.Properties.ItemNamed['ns'];
    PageTitle := BackLink.Properties.ItemNamed['title'];
    XML.Options := XML.Options - [sxoAutoCreate];
    Redirect := BackLink.Properties.ItemNamed['redirect'];
    XML.Options := XML.Options + [sxoAutoCreate];

    K := Length(Infos);
    SetLength(Infos, K + 1);
    Infos[K].BackLinkPageBasics.PageTitle := PageTitle.Value;
    Infos[K].BackLinkPageBasics.PageID := PageID.IntValue;
    Infos[K].BackLinkPageBasics.PageNamespace := PageNamespace.IntValue;
    if Assigned(Redirect) then
      Infos[K].BackLinkFlags := [mwfBackLinkIsRedirect]
    else
      Infos[K].BackLinkFlags := [];
    Infos[K].BackLinkRedirFromPageBasics.PageID := -1;
    Infos[K].BackLinkRedirFromPageBasics.PageNamespace := -1;
    Infos[K].BackLinkRedirFromPageBasics.PageTitle := '';

    RedirLinks := BackLink.Items.ItemNamed['redirlinks'];
    for J := 0 to RedirLinks.Items.Count - 1 do
    begin
      RedirLink := RedirLinks.Items.Item[J];

      RedirPageID := RedirLink.Properties.ItemNamed['pageid'];
      RedirPageNamespace := RedirLink.Properties.ItemNamed['ns'];
      RedirPageTitle := RedirLink.Properties.ItemNamed['title'];

      K := Length(Infos);
      SetLength(Infos, K + 1);
      Infos[K].BackLinkPageBasics.PageTitle := PageTitle.Value;
      Infos[K].BackLinkPageBasics.PageID := PageID.IntValue;
      Infos[K].BackLinkPageBasics.PageNamespace := PageNamespace.IntValue;
      Infos[K].BackLinkFlags := [mwfBackLinkIsRedirect, mwfBackLinkToRedirect];
      Infos[K].BackLinkRedirFromPageBasics.PageID := RedirPageID.IntValue;
      Infos[K].BackLinkRedirFromPageBasics.PageNamespace := RedirPageNamespace.IntValue;
      Infos[K].BackLinkRedirFromPageBasics.PageTitle := RedirPageTitle.Value;
    end;
  end;
end;

procedure MediaWikiQueryBackLinkParseXmlResult(XML: TJclSimpleXML; out StartBackLink: string);
var
  QueryContinue, BackLinks: TJclSimpleXMLElem;
  blContinue: TJclSimpleXMLProp;
begin
  StartBackLink := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  BackLinks := QueryContinue.Items.ItemNamed['backlinks'];
  if not Assigned(BackLinks) then
    Exit;
  blContinue := BackLinks.Properties.ItemNamed['blcontinue'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(blContinue) then
    StartBackLink := blContinue.Value;
end;

procedure MediaWikiQueryBlockAdd(Queries: TStrings; const StartDateTime, StopDateTime: TDateTime;
  const BlockIDs, Users, IP: string; MaxBlock: Integer; const StartBlock: string;
  Flags: TMediaWikiBlockInfoFlags; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'list', 'blocks');

  if StartDateTime <> 0.0 then
    MediaWikiQueryAdd(Queries, 'bkstart', DateTimeToStrISO8601(StartDateTime));

  if StopDateTime <> 0.0 then
    MediaWikiQueryAdd(Queries, 'bkend', DateTimeToStrISO8601(StopDateTime));

  if BlockIDs <> '' then
    MediaWikiQueryAdd(Queries, 'bkids', BlockIDs);

  if Users <> '' then
    MediaWikiQueryAdd(Queries, 'bkusers', Users);

  if IP <> '' then
    MediaWikiQueryAdd(Queries, 'bkip', IP);

  if MaxBlock > 0 then
    MediaWikiQueryAdd(Queries, 'bklimit', IntToStr(MaxBlock));

  if StartBlock <> '' then
    MediaWikiQueryAdd(Queries, 'bkstart', StartBlock, True);

  if mwfBlockID in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'id');
  if mwfBlockUser in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'user');
  if mwfBlockByUser in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'by');
  if mwfBlockDateTime in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'timestamp');
  if mwfBlockExpiry in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'expiry');
  if mwfBlockReason in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'reason');
  if mwfBlockIPRange in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'range');
  if mwfBlockFlags in Flags then
    MediaWikiQueryAdd(Queries, 'bkprop', 'flags');
  if mwfBlockDescending in Flags then
    MediaWikiQueryAdd(Queries, 'bkdir', 'older')
  else
    MediaWikiQueryAdd(Queries, 'bkdir', 'newer');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryBlockParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiBlockInfos);
var
  Query, Blocks, Block: TJclSimpleXMLElem;
  Index: Integer;
  ID, User, UserID, By, ByUserID, TimeStamp, Expiry, Reason, RangeStart, RangeEnd,
  Automatic, AnonOnly, NoCreate, AutoBlock, NoEmail, Hidden: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  Blocks := Query.Items.ItemNamed['blocks'];
  SetLength(Infos, Blocks.Items.Count);
  for Index := 0 to Blocks.Items.Count - 1 do
  begin
    Block := Blocks.Items.Item[Index];

    ID := Block.Properties.ItemNamed['id'];
    User := Block.Properties.ItemNamed['user'];
    UserID := Block.Properties.ItemNamed['userid'];
    By := Block.Properties.ItemNamed['by'];
    ByUserID := Block.Properties.ItemNamed['byuserid'];
    TimeStamp := Block.Properties.ItemNamed['timestamp'];
    Expiry := Block.Properties.ItemNamed['expiry'];
    Reason := Block.Properties.ItemNamed['reason'];
    RangeStart := Block.Properties.ItemNamed['rangestart'];
    RangeEnd := Block.Properties.ItemNamed['rangeend'];

    XML.Options := XML.Options - [sxoAutoCreate];
    Automatic := Block.Properties.ItemNamed['automatic'];
    AnonOnly := Block.Properties.ItemNamed['anononly'];
    NoCreate := Block.Properties.ItemNamed['nocreate'];
    AutoBlock := Block.Properties.ItemNamed['autoblock'];
    NoEmail := Block.Properties.ItemNamed['noemail'];
    Hidden := Block.Properties.ItemNamed['hidden'];
    XML.Options := XML.Options + [sxoAutoCreate];

    Infos[Index].BlockID := ID.IntValue;
    Infos[Index].BlockUser := User.Value;
    Infos[Index].BlockUserID := UserID.IntValue;
    Infos[Index].BlockByUser := By.Value;
    Infos[Index].BlockByUserID := ByUserID.IntValue;
    Infos[Index].BlockDateTime := StrISO8601ToDateTime(TimeStamp.Value);
    Infos[Index].BlockExpirityDateTime := StrISO8601ToDateTime(Expiry.Value);
    Infos[Index].BlockReason := Reason.Value;
    Infos[Index].BlockIPRangeStart := RangeStart.Value;
    Infos[Index].BlockIPRangeStop := RangeEnd.Value;
    Infos[Index].BlockFlags := [];
    if Assigned(Automatic) then
      Include(Infos[Index].BlockFlags, mwfBlockAutomatic);
    if Assigned(AnonOnly) then
      Include(Infos[Index].BlockFlags, mwfBlockAnonymousEdits);
    if Assigned(NoCreate) then
      Include(Infos[Index].BlockFlags, mwfBlockNoAccountCreate);
    if Assigned(AutoBlock) then
      Include(Infos[Index].BlockFlags, mwfBlockAutomaticBlocking);
    if Assigned(NoEmail) then
      Include(Infos[Index].BlockFlags, mwfBlockNoEmail);
    if Assigned(Hidden) then
      Include(Infos[Index].BlockFlags, mwfBlockHidden);
  end;
end;

procedure MediaWikiQueryBlockParseXmlResult(XML: TJclSimpleXML; out StartBlock: string);
var
  QueryContinue, Blocks: TJclSimpleXMLElem;
  bkStart: TJclSimpleXMLProp;
begin
  StartBlock := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  Blocks := QueryContinue.Items.ItemNamed['blocks'];
  if not Assigned(Blocks) then
    Exit;
  bkStart := Blocks.Properties.ItemNamed['bkstart'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(bkStart) then
    StartBlock := bkStart.Value;
end;

procedure MediaWikiQueryCategoryMemberAdd(Queries: TStrings; const CategoryTitle: string; PageNameSpace: Integer;
  const StartDateTime, StopDateTime: TDateTime; const StartSortKey, StopSortKey: string;
  MaxCategoryMember: Integer; const StartCategoryMember: string;
  Flags: TMediaWikiCategoryMemberInfoFlags; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'query');
  MediaWikiQueryAdd(Queries, 'list', 'categorymembers');

  if CategoryTitle <> '' then
    MediaWikiQueryAdd(Queries, 'cmtitle', CategoryTitle);

  if PageNamespace >= 0 then
    MediaWikiQueryAdd(Queries, 'cmnamespace', IntToStr(PageNamespace));
  
  if StartDateTime <> 0.0 then
  begin
    MediaWikiQueryAdd(Queries, 'cmsort', 'timestamp');
    MediaWikiQueryAdd(Queries, 'cmstart', DateTimeToStrISO8601(StartDateTime));
  end;

  if StopDateTime <> 0.0 then
    MediaWikiQueryAdd(Queries, 'cmend', DateTimeToStrISO8601(StopDateTime));

  if (StartSortKey <> '') and (StartDateTime = 0.0) and (StopDateTime = 0.0) then
  begin
    MediaWikiQueryAdd(Queries, 'cmsort', 'sortkey');
    MediaWikiQueryAdd(Queries, 'cmstartsortkey', StartSortKey);
  end;

  if (StopSortKey <> '') and (StartDateTime = 0.0) and (StopDateTime = 0.0) then
    MediaWikiQueryAdd(Queries, 'cmendsortkey', StopSortKey);

  if MaxCategoryMember > 0 then
    MediaWikiQueryAdd(Queries, 'cmlimit', IntToStr(MaxCategoryMember));

  if StartCategoryMember <> '' then
    MediaWikiQueryAdd(Queries, 'cmcontinue', StartCategoryMember, True);

  if mwfCategoryMemberPageID in Flags then
    MediaWikiQueryAdd(Queries, 'cmprop', 'ids');
  if mwfCategoryMemberPageTitle in Flags then
    MediaWikiQueryAdd(Queries, 'cmprop', 'title');
  if mwfCategoryMemberPageDateTime in Flags then
    MediaWikiQueryAdd(Queries, 'cmprop', 'timestamp');
  if mwfCategoryMemberPageSortKey in Flags then
    MediaWikiQueryAdd(Queries, 'cmprop', 'sortkey');
  if mwfCategoryMemberDescending in Flags then
    MediaWikiQueryAdd(Queries, 'cmdir', 'asc')
  else
    MediaWikiQueryAdd(Queries, 'cmdir', 'desc');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiQueryCategoryMemberParseXmlResult(XML: TJclSimpleXML; out Infos: TMediaWikiCategoryMemberInfos);
var
  Query, CategoryMembers, CategoryMember: TJclSimpleXMLElem;
  Index: Integer;
  PageID, NS, Title, SortKey, TimeStamp: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  Query := XML.Root.Items.ItemNamed['query'];
  CategoryMembers := Query.Items.ItemNamed['categorymembers'];
  SetLength(Infos, CategoryMembers.Items.Count);
  for Index := 0 to CategoryMembers.Items.Count - 1 do
  begin
    CategoryMember := CategoryMembers.Items.Item[Index];

    PageID := CategoryMember.Properties.ItemNamed['pageid'];
    NS := CategoryMember.Properties.ItemNamed['ns'];
    Title := CategoryMember.Properties.ItemNamed['title'];
    SortKey := CategoryMember.Properties.ItemNamed['sortkey'];
    TimeStamp := CategoryMember.Properties.ItemNamed['timestamp'];

    Infos[Index].CategoryMemberPageBasics.PageID := PageID.IntValue;
    Infos[Index].CategoryMemberPageBasics.PageNamespace := NS.IntValue;
    Infos[Index].CategoryMemberPageBasics.PageTitle := Title.Value;
    Infos[Index].CategoryMemberDateTime := StrISO8601ToDateTime(TimeStamp.Value);
    Infos[Index].CategoryMemberSortKey := SortKey.Value;
  end;
end;

procedure MediaWikiQueryCategoryMemberParseXmlResult(XML: TJclSimpleXML; out StartCategoryMember: string);
var
  QueryContinue, CategoryMembers: TJclSimpleXMLElem;
  cmStart, cmContinue: TJclSimpleXMLProp;
begin
  StartCategoryMember := '';
  // process continuation
  XML.Options := XML.Options - [sxoAutoCreate];
  QueryContinue := XML.Root.Items.ItemNamed['query-continue'];
  if not Assigned(QueryContinue) then
    Exit;
  CategoryMembers := QueryContinue.Items.ItemNamed['categorymembers'];
  if not Assigned(CategoryMembers) then
    Exit;
  cmStart := CategoryMembers.Properties.ItemNamed['cmstart'];
  cmContinue := CategoryMembers.Properties.ItemNamed['cmcontinue'];
  XML.Options := XML.Options + [sxoAutoCreate];

  if Assigned(cmStart) then
    StartCategoryMember := cmStart.Value
  else
  if Assigned(cmContinue) then
    StartCategoryMember := cmContinue.Value;
end;

procedure MediaWikiEditAdd(Queries: TStrings; const PageTitle, Section, Text, PrependText, AppendText, EditToken, Summary, MD5, CaptchaID, CaptchaWord: string;
  const BaseDateTime, StartDateTime: TDateTime; UndoRevisionID: TMediaWikiID; Flags: TMediaWikiEditFlags; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'edit');

  if PageTitle <> '' then
    MediaWikiQueryAdd(Queries, 'title', PageTitle);

  if Section <> '' then
    MediaWikiQueryAdd(Queries, 'section', Section);

  if Text <> '' then
    MediaWikiQueryAdd(Queries, 'text', Text);

  if PrependText <> '' then
    MediaWikiQueryAdd(Queries, 'prependtext', PrependText);

  if AppendText <> '' then
    MediaWikiQueryAdd(Queries, 'appendtext', AppendText);

  if EditToken <> '' then
    MediaWikiQueryAdd(Queries, 'token', EditToken);

  if Summary <> '' then
    MediaWikiQueryAdd(Queries, 'summary', Summary);

  if MD5 <> '' then
    MediaWikiQueryAdd(Queries, 'md5', MD5);

  if CaptchaID <> '' then
    MediaWikiQueryAdd(Queries, 'captchaid', CaptchaID);

  if CaptchaWord <> '' then
    MediaWikiQueryAdd(Queries, 'captchaword', CaptchaWord);

  if BaseDateTime <> 0.0 then
    MediaWikiQueryAdd(Queries, 'basetimestamp', DateTimeToStrISO8601(BaseDateTime));

  if StartDateTime <> 0.0 then
    MediaWikiQueryAdd(Queries, 'starttimestamp', DateTimeToStrISO8601(StartDateTime));

  if UndoRevisionID >= 0 then
    MediaWikiQueryAdd(Queries, 'undo', IntToStr(UndoRevisionID));

  if mwfEditMinor in Flags then
    MediaWikiQueryAdd(Queries, 'minor', 'true');
  if mwfEditNotMinor in Flags then
    MediaWikiQueryAdd(Queries, 'notminor', 'true');
  if mwfEditBot in Flags then
    MediaWikiQueryAdd(Queries, 'bot');
  if mwfEditAlwaysRecreate in Flags then
    MediaWikiQueryAdd(Queries, 'recreate');
  if mwfEditMustCreate in Flags then
    MediaWikiQueryAdd(Queries, 'createonly');
  if mwfEditMustExist in Flags then
    MediaWikiQueryAdd(Queries, 'nocreate');
  if mwfEditWatchAdd in Flags then
    MediaWikiQueryAdd(Queries, 'watchlist', 'watch')
  else
  if mwfEditWatchRemove in Flags then
    MediaWikiQueryAdd(Queries, 'watchlist', 'unwatch')
  else
  if mwfEditWatchNoChange in Flags then
    MediaWikiQueryAdd(Queries, 'watchlist', 'nochange')
  else
    MediaWikiQueryAdd(Queries, 'watchlist', 'preferences');
  if mwfEditUndoAfterRev in Flags then
    MediaWikiQueryAdd(Queries, 'undoafter');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiEditParseXmlResult(XML: TJclSimpleXML; out Info: TMediaWikiEditInfo);
var
  EditNode, CaptchaNode: TJclSimpleXMLElem;
  ResultProp, TitleProp, PageIDProp, OldRevIDProp, NewRevIDProp,
  CaptchaTypeProp, CaptchaURLProp, CaptchaMimeProp, CaptchaIDProp, CaptchaQuestionProp: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  EditNode := XML.Root.Items.ItemNamed['edit'];
  CaptchaNode := EditNode.Items.ItemNamed['captcha'];

  ResultProp := EditNode.Properties.ItemNamed['result'];
  TitleProp := EditNode.Properties.ItemNamed['title'];
  PageIDProp := EditNode.Properties.ItemNamed['pageid'];
  OldRevIDProp := EditNode.Properties.ItemNamed['oldrevid'];
  NewRevIDProp := EditNode.Properties.ItemNamed['newrevid'];

  CaptchaTypeProp := CaptchaNode.Properties.ItemNamed['type'];
  CaptchaURLProp := CaptchaNode.Properties.ItemNamed['url'];
  CaptchaMimeProp := CaptchaNode.Properties.ItemNamed['mime'];
  CaptchaIDProp := CaptchaNode.Properties.ItemNamed['id'];
  CaptchaQuestionProp := CaptchaNode.Properties.ItemNamed['question'];

  Info.EditSuccess := ResultProp.Value = 'Success';
  Info.EditPageTitle := TitleProp.Value;
  Info.EditPageID := PageIDProp.IntValue;
  Info.EditOldRevID := OldRevIDProp.IntValue;
  Info.EditNewRevID := NewRevIDProp.IntValue;

  Info.EditCaptchaType := CaptchaTypeProp.Value;
  Info.EditCaptchaURL := CaptchaURLProp.Value;
  Info.EditCaptchaMime := CaptchaMimeProp.Value;
  Info.EditCaptchaID := CaptchaIDProp.Value;
  Info.EditCaptchaQuestion := CaptchaQuestionProp.Value;
end;

procedure MediaWikiMoveAdd(Queries: TStrings; const FromPageTitle, ToPageTitle, MoveToken, Reason: string;
  FromPageID: TMediaWikiID; Flags: TMediaWikiMoveFlags; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'move');

  if FromPageTitle <> '' then
    MediaWikiQueryAdd(Queries, 'from', FromPageTitle);

  if ToPageTitle <> '' then
    MediaWikiQueryAdd(Queries, 'to', ToPageTitle);

  if MoveToken <> '' then
    MediaWikiQueryAdd(Queries, 'token', MoveToken);

  if Reason <> '' then
    MediaWikiQueryAdd(Queries, 'reason', Reason);

  if (FromPageID >= 0) and (FromPageTitle = '') then
    MediaWikiQueryAdd(Queries, 'fromid', IntToStr(FromPageID));

  if mwfMoveTalk in Flags then
    MediaWikiQueryAdd(Queries, 'movetalk');
  if mwfMoveSubPages in Flags then
    MediaWikiQueryAdd(Queries, 'movesubpages');
  if mwfMoveNoRedirect in Flags then
    MediaWikiQueryAdd(Queries, 'noredirect');
  if mwfMoveAddToWatch in Flags then
    MediaWikiQueryAdd(Queries, 'watch')
  else
  if mwfMoveNoWatch in Flags then
    MediaWikiQueryAdd(Queries, 'unwatch');

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiMoveParseXmlResult(XML: TJclSimpleXML; out Info: TMediaWikiMoveInfo);
var
  MoveNode: TJclSimpleXMLElem;
  FromProp, ToProp, ReasonProp, TalkFromProp, TalkToProp: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  MoveNode := XML.Root.Items.ItemNamed['move'];

  FromProp := MoveNode.Properties.ItemNamed['from'];
  ToProp := MoveNode.Properties.ItemNamed['to'];
  ReasonProp := MoveNode.Properties.ItemNamed['reason'];
  TalkFromProp := MoveNode.Properties.ItemNamed['talkfrom'];
  TalkToProp := MoveNode.Properties.ItemNamed['talkto'];

  Info.MoveSuccess := ToProp.Value <> '';
  Info.MoveFromPage := FromProp.Value;
  Info.MoveToPage := ToProp.Value;
  Info.MoveReason := ReasonProp.Value;
  Info.MoveFromTalk := TalkFromProp.Value;
  Info.MoveToTalk := TalkToProp.Value;
end;

procedure MediaWikiDeleteAdd(Queries: TStrings; const PageTitle, DeleteToken, Reason: string;
  PageID: TMediaWikiID; OutputFormat: TMediaWikiOutputFormat);
begin
  MediaWikiQueryAdd(Queries, 'action', 'delete');

  if PageTitle <> '' then
    MediaWikiQueryAdd(Queries, 'title', PageTitle);

  if DeleteToken <> '' then
    MediaWikiQueryAdd(Queries, 'token', DeleteToken);

  if Reason <> '' then
    MediaWikiQueryAdd(Queries, 'reason', Reason);

  if (PageID >= 0) and (PageTitle = '') then
    MediaWikiQueryAdd(Queries, 'pageid', IntToStr(PageID));

  MediaWikiQueryAdd(Queries, 'format', MediaWikiOutputFormats[OutputFormat]);
end;

procedure MediaWikiDeleteParseXmlResult(XML: TJclSimpleXML; out Info: TMediaWikiDeleteInfo);
var
  DeleteNode: TJclSimpleXMLElem;
  TitleProp, ReasonProp: TJclSimpleXMLProp;
begin
  XML.Options := XML.Options + [sxoAutoCreate];
  DeleteNode := XML.Root.Items.ItemNamed['delete'];

  TitleProp := DeleteNode.Properties.ItemNamed['title'];
  ReasonProp := DeleteNode.Properties.ItemNamed['reason'];

  Info.DeleteSuccess := TitleProp.Value <> '';
  Info.DeletePage := TitleProp.Value;
  Info.DeleteReason := ReasonProp.Value;
end;

end.

