%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cup-elements.cls, v1.1, 2019/03/07
% Developed by Overleaf.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
\ProvidesClass{cup-elements}[2018/11/28, v1.0]
\LoadClass[10pt,twoside]{article}

\RequirePackage{etoolbox}
\RequirePackage{regexpatch}
\RequirePackage{ifxetex,ifluatex}

\newif\ifxetexorluatex
\ifxetex
  \xetexorluatextrue
\else
  \ifluatex
    \xetexorluatextrue
  \else
    \xetexorluatexfalse
  \fi
\fi

%% 7 March, 2019: Equations are now centred by CUP request
\RequirePackage{amsmath}
%% 7 March, 2019: Packages added by CUP request
\RequirePackage{amsxtra}
\RequirePackage{amssymb}
\RequirePackage{xspace}
\RequirePackage{url}

\RequirePackage{microtype}

\ifxetexorluatex
  \RequirePackage[libertine]{newtxmath}
  \RequirePackage[no-math]{fontspec}
  \setmainfont{TeX Gyre Termes}
  \setsansfont[Extension=.ttf,
    UprightFont=*-Regular,
    BoldFont=*-Bold,
    ItalicFont=*-Italic,
    BoldItalicFont=*-BoldItalic]{OpenSans}
%   \setsansfont[UprightFont=Lato Regular,
%     BoldFont=Lato Bold,
%     ItalicFont=Lato Italic,
%     BoldItalicFont=Lato Bold Italic]{Lato}
% Patch microtype for older versions; from https://tex.stackexchange.com/a/373607/226
  \@ifpackagelater{microtype}{2017/07/05}{}{%
    \patchcmd{\MT@is@composite}
      {\expandafter\expandafter\expandafter}
      {\ifx\UnicodeEncodingName\@undefined\else
      \expandafter\expandafter\expandafter\MT@is@uni@comp\MT@char\iffontchar\else\fi\relax
    \fi\expandafter\expandafter\expandafter}
      {}{}
    \def\MT@is@uni@comp#1\iffontchar#2\else#3\fi\relax{%
      \ifx\\#2\\\else\edef\MT@char{\iffontchar#2\fi}\fi
    }
  }
\else
  \RequirePackage[utf8]{inputenc}
  \RequirePackage[T1]{fontenc}
  \RequirePackage{newtxtext,newtxmath}
%   \RequirePackage[defaultsans]{lato}
  \RequirePackage[defaultsans]{opensans}
\fi

\RequirePackage{ragged2e}
\RequirePackage{geometry}
\RequirePackage{fancyhdr}
\RequirePackage[explicit]{titlesec}
\RequirePackage{tocloft}
\RequirePackage{enumitem}
\RequirePackage{caption}
\RequirePackage{tabularx}
\RequirePackage{chngcntr}
\RequirePackage[framemethod=tikz]{mdframed}
\RequirePackage{quoting}
\RequirePackage{varwidth}


%% Body font size is 10/14pt
\patchcmd{\normalsize}{\@xiipt}{{14}}{}{}
\appto{\normalsize}{%
  \setlength{\abovedisplayskip}{0.5\baselineskip}%
  \setlength{\belowdisplayskip}{0.5\baselineskip}%
  \setlength{\abovedisplayshortskip}{0.5\baselineskip}%
  \setlength{\belowdisplayshortskip}{0.5\baselineskip}%
}
\normalsize

%% Page trim size and margins
\geometry{paperwidth=152truemm,paperheight=229truemm,
  textwidth=26truepc,inner=4.5pc,
  lines=40,includehead,top=\dimexpr 13truemm-1em\relax,
  headsep=\dimexpr 24\p@-1em\relax,
}

\def\cup@series{Series Name as Running Head}
\newcommand{\CUPseries}[1]{\def\cup@series{#1}}
\def\cup@elements{Elements Name as Running Head}
\newcommand{\CUPelements}[1]{\def\cup@elements{#1}}

%% Running page headers
\fancyhf{}
\renewcommand{\headrule}{}
\fancyhead[RO,LE]{\thepage}
\fancyhead[CE]{\sffamily\itshape\cup@series}
\fancyhead[CO]{\sffamily\itshape\cup@elements}

\fancypagestyle{appsection}{%
  \fancyhead[C]{}
}

%% Sectional headers
\titleformat{\section}{\filcenter\large\sffamily\bfseries}{\thesection}{1ex}{#1}
\titlespacing{\section}{\z@}{\dimexpr 26\p@-1em}{\dimexpr 20\p@-1em}

\titleformat{\subsection}{\filcenter\large\sffamily}{\thesubsection}{1ex}{#1}
\titlespacing{\subsection}{\z@}{\dimexpr 26\p@-1em}{\dimexpr 20\p@-1em}

\titleformat{\subsubsection}{\filcenter\fontsize{10\p@}{12\p@}\selectfont\sffamily\itshape}{\thesubsubsection}{1ex}{#1}
\titlespacing{\subsubsection}{\z@}{\dimexpr 28\p@-1em}{\dimexpr 20\p@-1em}

\titleformat{\paragraph}{\filcenter\fontsize{10\p@}{12\p@}\selectfont\sffamily\bfseries}{}{\z@}{#1}
\titlespacing{\paragraph}{\z@}{\dimexpr 28\p@-1em}{\dimexpr 17\p@-1em}

\setcounter{tocdepth}{1}

\appto{\appendix}{%
  \addtocontents{toc}{\protecting{\setcounter{tocdepth}{0}}}%
  \titleformat{name=\section,numberless}
    {\clearpage%
    \filcenter\fontsize{16\p@}{18\p@}\selectfont%
    \sffamily\bfseries}{}{1ex}{#1}
    [\thispagestyle{appsection}%
     \chead{\sffamily\itshape #1}]
  
  \titlespacing{\section}{\z@}{\dimexpr 26\p@-1em}{\dimexpr 20\p@-1em}

  \titleformat{name=\section}[display]%
    {\clearpage\filcenter\fontsize{16\p@}{18\p@}\selectfont\sffamily\bfseries}%
    {\appendixname~\thesection}%
    {\dimexpr 24\p@-1em}{\itshape\mdseries #1}
    [\thispagestyle{appsection}%
     \chead{\sffamily\itshape\appendixname~\thesection}]%
    
  \renewcommand{\theequation}{\thesection-\arabic{equation}}%
  %
  \renewcommand{\thesubsection}{\thesection\arabic{subsection}}%
  \titleformat{\subsection}{\filcenter\large\sffamily\bfseries}{\thesubsection}{1ex}{#1}%
  \titlespacing{\subsection}{\z@}{\dimexpr 26\p@-1em}{\dimexpr 20\p@-1em}%
%
  \titleformat{\subsubsection}{\filcenter\large\sffamily}{\thesubsubsection}{1ex}{#1}%
  \titlespacing{\subsubsection}{\z@}{\dimexpr 26\p@-1em}{\dimexpr 20\p@-1em}%
%
  \titleformat{\paragraph}{\filcenter\fontsize{10\p@}{12\p@}\selectfont\sffamily\itshape}{}{\z@}{#1}%
  \titlespacing{\paragraph}{\z@}{\dimexpr 28\p@-1em}{\dimexpr 20\p@-1em}%
%
  \titleformat{\subparagraph}{\filcenter\fontsize{10\p@}{12\p@}\selectfont\sffamily\bfseries}{}{\z@}{#1}
  \titlespacing{\subparagraph}{\z@}{\dimexpr 28\p@-1em}{\dimexpr 17\p@-1em}%
}

%% Maths-related
\counterwithin{equation}{section}
%% 7 March, 2019: Equations are now centred by CUP request, so this is now unnecessary
% \setlength{\mathindent}{1truepc}

%% Figures and tables
\DeclareCaptionFont{narrownormal}{\fontsize{10\p@}{12\p@}\selectfont}
\captionsetup*{singlelinecheck=false,justification=centering,labelfont=bf,labelsep=space,skip=2.8\p@}
\captionsetup*[table]{font=narrownormal,position=top}
\renewcommand{\tabularxcolumn}[1]{>{\RaggedRight\setlength{\hangindent}{1em}\arraybackslash}p{#1}}

\newcommand{\addnote}[1]{\vspace*{2.8\p@}\justifying\noindent\fontsize{9\p@}{12\p@}\selectfont\textbf{Note: }#1\par}
\newcommand{\addsource}[1]{\vspace*{2.8\p@}\justifying\noindent\fontsize{9\p@}{12\p@}\selectfont\textbf{Source: }#1\par}

\newcommand{\topdblline}{\hline\hline\rule{0pt}{2em}}
\newcommand{\bottomdblline}{\hline\hline}

\newcommand{\patchtableenv}[1]{%
  \AtBeginEnvironment{#1}{%
    \fontsize{10\p@}{13\p@}\selectfont%
    \renewcommand{\arraystretch}{1.2}%
  }%
}
\patchtableenv{tabular}
\patchtableenv{tabularx}


%% Lists
\setlength{\parindent}{1pc}
\setlist{noitemsep,leftmargin=*}

%% Quotations and boxes
\let\quote\quoting
\let\endquote\endquoting
\let\quotation\quoting
\let\endquotation\endquoting
\quotingsetup{leftmargin=20\p@,begintext={\fontsize{9\p@}{12\p@}\selectfont},vskip=0.5\baselineskip,indentfirst=false}

\newenvironment{boxedtext}[1]{%
  \begin{mdframed}[frametitle=#1,
    frametitlefont=\scshape\mdseries\sffamily,
    frametitlealignment=\centering,
    backgroundcolor=black!20,
    hidealllines=true,
    innerleftmargin=11\p@,innerrightmargin=11\p@,
    frametitleaboveskip=0.5\baselineskip,
    innerbottommargin=0.5\baselineskip,
    skipabove=\baselineskip,skipbelow=0.5\baselineskip]
}{%
  \end{mdframed}%
}

\RequirePackage[pagewise,mathlines]{lineno}
% \linenumbers

% \newcommand\linenomathpatch{\@ifstar{\linenomathpatch@1}{\linenomathpatch@0}}
% \newcommand\linenomathpatch@[2]{
%   \ifnum#1=1
%     \expandafter\pretocmd\csname #2\endcsname {\linenomathWithnumbersAMS}{}{}
%     \expandafter\pretocmd\csname #2*\endcsname{\linenomathWithnumbersAMS}{}{}
%   \else
%     \expandafter\pretocmd\csname #2\endcsname {\linenomathWithnumbers}{}{}
%     \expandafter\pretocmd\csname #2*\endcsname{\linenomathWithnumbers}{}{}
%   \fi
%   \expandafter\apptocmd\csname end#2\endcsname {\endlinenomath}{}{}
%   \expandafter\apptocmd\csname end#2*\endcsname{\endlinenomath}{}{}
% }
% \newcommand\linenomathWithnumbersAMS{%
%   \ifLineNumbers
%     \ifnum\interlinepenalty>-\linenopenaltypar
%       \global\holdinginserts\thr@@
%       \advance\interlinepenalty \linenopenalty
%      \ifhmode
%       \advance\predisplaypenalty \linenopenalty
%      \fi
%       % \advance\postdisplaypenalty \linenopenalty %% <- removed
%       \advance\interdisplaylinepenalty \linenopenalty
%     \fi
%   \fi
%   \ignorespaces
% }

% \linenomathpatch{equation}
% \linenomathpatch*{gather}
% \linenomathpatch*{multline}
% \linenomathpatch*{align}
% \linenomathpatch*{alignat}
% \linenomathpatch*{flalign}

%% The title page
\def\@subtitle{}
\def\cup@meta{%
  DOI: 10.xxxx/xxxxxxxx (do not change)\\
  First published online: MMM dd YYYY (do not change)%
}
\newcommand{\subtitle}[1]{\def\@subtitle{#1}}

\def\@author{}
\renewcommand{\author}[1]{\appto{\@author}{{\small\sffamily #1\par}}}
\newcommand{\affil}[1]{\appto{\@author}{{\small\sffamily\itshape #1\par}\vspace*{6.7\p@}}}

\newcommand{\keywords}[1]{\vspace*{6.7\p@}{\small\textbf{Keywords: }#1\par}}

\newcommand{\JEL}[1]{\vspace*{6.7\p@}{\small\textbf{JEL classifications: }#1\par}}

\newcommand{\copyrightauthor}[1]{\vfill{\small\textcopyright\ #1\par}}

\renewcommand{\maketitle}{%
  \centering\sffamily\vspace*{37\p@}
  \fontsize{13\p@}{18\p@}\bfseries\@title\par
  \ifdefempty{\@subtitle}{}{%
    \vspace*{18\p@}%
    \fontsize{11\p@}{14\p@}\bfseries\@subtitle\par
  }
  \vspace*{20\p@}
  {\small\cup@series\par}
   \vspace*{18\p@}
  \fontsize{11\p@}{14\p@}\mdseries\cup@meta\par
  \vspace*{\baselineskip}\hrule\vspace*{\baselineskip}
  \@author\par
}

\newcommand{\frontmatter}{%
  \pagestyle{empty}%
  \begingroup\titlepage%
  \newgeometry{textwidth=104.5truemm,hcentering,
  lines=40,includehead,top=\dimexpr 13truemm-1em\relax,
  headsep=\dimexpr 24\p@-1em\relax,
}

}
\newcommand{\mainmatter}{%
  \vspace*{7\p@}
  \begin{minipage}{\textwidth}\centering\footnotesize%
   ISBNs: xxxxxxxxxxxxx(PB) xxxxxxxxxxxxx(OC)\\%
   ISSNs: xxxx-xxxx (online) xxxx-xxxx (print)\par
   \end{minipage}
  \endtitlepage\endgroup%
  \restoregeometry%
  \tableofcontents\clearpage%
  \pagenumbering{arabic}%
  \pagestyle{fancy}%
}

\renewenvironment{abstract}{%
  \begin{mdframed}[%
    backgroundcolor=black!20,font=\small,
    hidealllines=true,
    usetwoside=false,
    leftmargin=-3truemm,rightmargin=-3truemm,
    innerleftmargin=3truemm,innerrightmargin=3truemm,
    innertopmargin=3truemm,innerbottommargin=3truemm,
    skipabove=\z@,skipbelow=\z@]
    \centering\textbf{\abstractname: }\ignorespaces%
}{%
  \end{mdframed}\vspace*{-0.5\baselineskip}%
}

%% Table of contents

\AtBeginDocument{%
  \@ifpackageloaded{natbib}{%
    \renewcommand{\bibpreamble}{%
      \addtocontents{toc}{\protecting{\setcounter{tocdepth}{1}}}%
      \ltx@ifpackageloaded{hyperref}{\phantomsection}{}
      \addcontentsline{toc}{section}{\hspace*{\cftsecnumwidth}\refname}%
    }}{%
      \AtBeginEnvironment{thebibliography}{%
        \addtocontents{toc}{\protecting{\setcounter{tocdepth}{1}}}%
      }%
      \patchcmd{\thebibliography}{\section*{\refname}}
        {\section*{\refname}%
         \ltx@ifpackageloaded{hyperref}{\phantomsection}{}%
         \addcontentsline{toc}{section}{\hspace*{\cftsecnumwidth}\refname}}
        {}{}
   }
}

\renewcommand{\cfttoctitlefont}{\clearpage\hfill\fontsize{16\p@}{18\p@}\selectfont\sffamily\bfseries}
\renewcommand{\cftaftertoctitle}{%
  \hfill\null\pagestyle{empty}\pagenumbering{gobble}%
}
\setlength{\cftaftertoctitleskip}{75\p@}
\setlength{\cftbeforesecskip}{\baselineskip}
\renewcommand{\cftsecfont}{\sffamily\large}
\renewcommand{\cftsecpagefont}{\sffamily\large}
\DeclareRobustCommand{\vw}[1]{%
  \begin{varwidth}[t]{0.75\textwidth}%
  \raggedright\strut#1\unskip\nobreak\strut\par
  \end{varwidth}%
}
\patchcmd{\ttl@addcontentsline}{#2}{\vw{#2}}{}{}


\endinput