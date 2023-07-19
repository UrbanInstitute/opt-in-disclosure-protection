% Urban Institute Short Report Formatting for LaTeX
%
% Based on the Urban Institute's formatting standards for short reports.
%
% ==============================
% Copyright 2021 Claire McKay Bowen, Ph.D.
% ==============================
%
% Notes -----------------------------------
% \newcommand defines a new command, and makes an error if it is already defined.
% \renewcommand redefines a predefined command, and makes an error if it is not yet defined.
% \providecommand defines a new command if it isn't already defined.
%
% \newcommand{<name>}[<args>]{ <code> }
% \fontsize{10}{12}\selectfont (the first number is the pt size of the font, the second number the space in pts between lines: this becomes the value of \baselineskip
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Meta Data and Defaults %%%%%%%%%%%%%%%%%%%%%%%%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{urban-formatting}
\LoadClass[twoside]{article} 
\usepackage[top = 1.13in, left = 1.13in, bottom = 1.2in, right = 1.13in, paperheight = 11in, paperwidth = 8.5in]{geometry}

% Packages -----------------------------------
\RequirePackage{% Default
    array, % Table Column Width
    caption, % Figure/Table Caption Formatting
    changepage, % Changing margin
    color, % Color text
    colortbl, % Color hline
    csquotes, % Typesets quotes
    endnotes, % Footnote Formatting
    enumitem, % Itemize Formatting
    fancyhdr, % Header/Footer Formatting
    graphicx, % Figure package
    multicol, % Create Multi-columns
    multirow, % Create Multi-rows
    pdfpages, % Insert pdfs
    ragged2e, % Alignment 
    scrextend, % Adding margin
    setspace, % Line Spacing
    subcaption, % Figure/Table Subaption Formatting
    tabularx, % Table Management
    titlesec, % Change Formatting
    tocloft, % For ToC Formatting
    xcolor % Defining new colors
}

% Color Box for footers, headers, etc.
\RequirePackage[most]{tcolorbox}
% Absolute positioning of images (e.g., title page footer image)
\RequirePackage[absolute]{textpos}
% Chicago Reference
\RequirePackage[authordate, autocite = inline, backend = biber, isbn = false, natbib]{biblatex-chicago}
% Colored URL
\usepackage[colorlinks = true,
            linkcolor = black,
            urlcolor  = urban-url,
            citecolor = black,
            anchorcolor = black]{hyperref}
% Letter spacing
% Remove the colon
\captionsetup[table]{labelsep=none}
\captionsetup[figure]{labelsep=none}

% Urban Colors -----------------------------------
\definecolor{urban-cyan}{HTML}{00aeef}
\definecolor{urban-blue}{HTML}{1696d2}
\definecolor{urban-url}{HTML}{0a4c6a}
\definecolor{urban-gold}{HTML}{ffb918}
\definecolor{urban-gray}{HTML}{9d9fa2}
\definecolor{urban-darkgray}{HTML}{5c5859}
\definecolor{urban-footergray}{HTML}{c6c6c6}
\definecolor{urban-lightgray}{HTML}{d9d9d9}

% General Formatting -----------------------------------
\renewcommand{\baselinestretch}{1.5} % 1.5 spacing
\raggedright % Left justified
\setlength{\parindent}{0.25in} % Paragraph indents
\setlength{\parskip}{9pt} % Paragraph spacing
\setcounter{tocdepth}{2} % TOC listing depth
\setcounter{secnumdepth}{-1} % Suppressing numbering on sections
\let\footnote=\endnote % Changing footnotes to endnotes
\setlength{\unitlength}{1in} % Setting unit length for creating the footers
% Fixing the hyphenation issues
\emergencystretch=\maxdimen
\hyphenpenalty=10000


%% for all publications
\newcommand{\Address}[1]{\def\@Address{#1}}
\newcommand{\Plaintitle}[1]{\def\@Plaintitle{#1}}
\newcommand{\Shorttitle}[1]{\def\@Shorttitle{#1}}
\newcommand{\Plainauthor}[1]{\def\@Plainauthor{#1}}
\newcommand{\Volume}[1]{\def\@Volume{#1}}
\newcommand{\Year}[1]{\def\@Year{#1}}
\newcommand{\Month}[1]{\def\@Month{#1}}
\newcommand{\Issue}[1]{\def\@Issue{#1}}
\newcommand{\Submitdate}[1]{\def\@Submitdate{#1}}
%% for articles and code snippets
\newcommand{\Acceptdate}[1]{\def\@Acceptdate{#1}}
\newcommand{\Abstract}[1]{\def\@Abstract{#1}}
\newcommand{\Keywords}[1]{\def\@Keywords{#1}}
\newcommand{\Plainkeywords}[1]{\def\@Plainkeywords{#1}}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Header/Footer %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Urban Gray Bar
\tcbset{
    frame code = {}
    center title,
    left = 0pt,
    right = 0pt,
    top = 5pt,
    bottom = 5pt,
    colback = urban-lightgray,
    colframe = white,
    width=\dimexpr\textwidth\relax,
    enlarge left by=0mm,
    boxsep = 5pt,
    arc=0pt,outer arc=0pt,
}
\pagestyle{fancy}

% Clear the header and remove horizontal line
\fancyhead{}
\renewcommand{\headrulewidth}{0pt}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TOC %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Title Formatting
\renewcommand{\cfttoctitlefont}{\color{urban-blue}\fontsize{28}{0}\selectfont\raggedright}

% Font Style
\renewcommand{\cftsecfont}{\fontsize{11}{0}\selectfont}
\renewcommand{\cftsubsecfont}{\fontsize{10}{0}\selectfont}
\renewcommand{\cftsecpagefont}{\normalfont}
\renewcommand{\cftdot}{}

% Indents
\cftsetindents{section}{0.2in}{0.5in}
\cftsetindents{subsection}{0.4in}{0.5in}

% Spacing
\setlength{\cftbeforeloftitleskip}{0in}
\setlength{\cftbeforepartskip}{16pt}
\setlength{\cftbeforesecskip}{8pt}
\setlength{\cftbeforesubsecskip}{8pt}

\renewcommand\cftpartafterpnum{\vskip-4pt}
\renewcommand\cftsecafterpnum{\vskip-4pt}
\renewcommand\cftsubsecafterpnum{\vskip-4pt}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Bullets %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Spacing between bullet and text
\setlist[itemize, 1]{leftmargin = 0.5in, labelsep = 4.5mm}
\setlist[enumerate, 1]{leftmargin = 0.5in, labelsep = 3mm}

\setlist[enumerate, 2]{label=\alph*.}

% Blue Square Bullet
\newcommand{\squarebullet}{\textcolor{urban-blue}{\raisebox{.5ex}{\rule{0.7ex}{0.7ex}}}}
\renewcommand{\labelitemi}{\squarebullet}

% Double arrow bullet
\newcommand{\doublearrow}{\fontseries{mb}\selectfont >>}
\renewcommand{\labelitemii}{\doublearrow}

% Spacing between items
\setlist{nosep}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Quote %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newcommand{\quoteblock}[1]{
    \begin{addmargin}[0.5in]{0in}
        #1
    \end{addmargin}
}

\newcommand\pullquote[1]{%
    {%
        \fontsize{12}{20}%
        \linespread{1}%
        \selectfont%
        \noindent%
        \textcolor{urban-blue}{#1}%
    }%
}%

% Horizontal Bars for pull quotes
\newcommand{\prequotebar}{%
    \par\addvspace{30pt}%
    \begingroup\color{urban-blue}%
        \hrule%
    \endgroup%
    \addvspace{10pt}%
}

\newcommand{\postquotebar}{%
    \addvspace{20pt}%
    \begingroup\color{urban-blue}%
        \hrule%
    \endgroup%
    \addvspace{30pt}%
}

% Block Text Style Formatting
\newcommand{\block}[1]{%
    {%
        \begin{adjustwidth}{0.5in}{0pt}
            \fontsize{9}{0}%
            \selectfont%
            \noindent%
            #1%
        \end{adjustwidth}
    }%
}%

% Horizontal Bars for box quotes
\newcommand{\preboxbar}{%
    \par\addvspace{24pt}%
    \begingroup\color{urban-footergray}%
        \hrule height 5pt%
    \endgroup%
    \addvspace{6pt}%
}

\newcommand{\postboxbar}{%
    \addvspace{12pt}%
    \begingroup\color{urban-footergray}%
        \hrule height 5pt%
    \endgroup%
    \addvspace{24pt}%
}

% Box Quote Head
\newcommand\boxhead[1]{%
    {%
        \fontsize{9}{0}%
        \selectfont%
        \noindent%
        \textcolor{urban-blue}{\MakeUppercase{#1}}%
    }%
}%

% Box Quote Title
\newcommand\boxtitle[1]{%
    {%
        \fontsize{10}{0}%
        \fontseries{b}%
        \selectfont%
        \noindent%
        #1
    }%
}%

% Box Quote Note
\newcommand\boxnote[1]{%
    {%
        \begin{singlespace}
            \fontsize{8}{0}%
            \selectfont%
            \noindent%
            #1
        \end{singlespace}
    }%
}%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fonts %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\renewcommand{\familydefault}{\sfdefault} % Family Font as Lato
% https://tug.org/FontCatalogue/lato/

% Title Call
\def\title#1{\gdef\@title{#1}\gdef\THETITLE{#1}}

% To be filled
\newcommand{\tbf}{\textcolor{red}}%

% Defining set fonts
\def\normalsize{\fontsize{10}{12}}% 18 to 12 (divide by 1.5 spacing)
\def\enotesize{\fontsize{9}{9}}

% Part Formatting - H1
\titleformat{\part}[display]{\color{urban-blue}}{\thepart}{}{\fontsize{28}{24}\selectfont\raggedright}% 36 to 26 (divide by 1.5 spacing)
\titlespacing*{\part}{0pt}{-28pt}{0pt}%

% Section Formatting - H2
\titleformat{\section}[display]{}{}{}{\fontsize{18}{16}\selectfont\raggedright}% 24 to 16 (divide by 1.5 spacing)
\titlespacing*{\section}{0pt}{21pt}{4pt}% 32 to 21, 6 to 4 (divide by 1.5 spacing)

% Subsection Formatting - H3
\titleformat{\subsection}[display]{\bfseries}{}{}{\fontsize{12}{18}\selectfont\raggedright}%
\titlespacing*{\subsection}{0pt}{12pt}{-2pt}% 18 to 12, 6 to -2 (divide by 1.5 spacing)

% Subsubsection Formatting - H4
\titleformat{\subsubsection}[display]{\bfseries\color{urban-gray}}{}{0em}{\fontsize{10}{12}\selectfont\MakeUppercase\raggedright}% 18 to 12, 6 to 4 (divide by 1.5 spacing)
\titlespacing*{\subsubsection}{0pt}{8pt}{-8pt}% 12 to 8 (divide by 1.5 spacing), no spacing afterward

% fourthsub Formatting - H5
\newcommand\fourthsub[1]{%
    {%
        \fontsize{10}{0}%
        \fontseries{b}%
        \textit{#1}%
        \selectfont%
    }%
}%

% Policy Center Formatting
\newcommand\policycenter[1]{%
    {%
        \fontsize{8}{0}%
        \fontseries{b}%
        \selectfont%
        \noindent%
        \textcolor{urban-darkgray}{\MakeUppercase{#1}}%
    }%
}%

% Title Page
\newcommand\boom[1]{
    \begin{textblock*}{9in}(-0.25in, 0.15in)
        \begin{tcolorbox}[valign = center]
            \begin{center}
                \policycenter{#1}
            \end{center}
        \end{tcolorbox}
    \end{textblock*}
}


% Title Report Formatting
\newcommand\titlereport[1]{%
    {%
    \begin{doublespace}%
        \fontsize{30pt}{38pt}%
        \fontseries{l}%
        \linespread{1}%
        \selectfont%
        \noindent#1%
    \end{doublespace}%
    }%
}%

% Report Subtitle Formatting
\newcommand\reportsubtitle[1]{%
    {%
        \fontsize{14}{18pt}%
        \fontseries{b}%
        \linespread{1}%
        \selectfont%
        \noindent%
        \textcolor{urban-blue}{#1}%
    }%
}%

% Author Name Formatting
\newcommand\authorfont[1]{%
    \fontsize{11.5}{14}%
    \selectfont%
    \noindent%
    \textit{#1}%
}%

% Author Affiliation Formatting
\newcommand\affiliationfont[1]{%
    \fontsize{7.5}{7.5}%
    \selectfont%
    \noindent%
    \textbf{\MakeUppercase{#1}}%
}%

% Date Formatting
\newcommand\datefont[1]{%
    \fontsize{11.5}{7}%
    \selectfont%
    \noindent\textcolor{urban-darkgray}{\textit{#1}}%
}%

% About Urban Formatting
\newcommand\about[1]{%
    {%
        \fontsize{10}{18}%
        \fontseries{b}%
        \selectfont%
        \noindent
        \textcolor{urban-blue}{\MakeUppercase{#1}}%
    }%
}%

% Boiler Plate Formatting
\newcommand*{\boilerplate}[1]{%
    \begin{singlespace}
        {\fontsize{9}{12}%
        \selectfont%
        \noindent#1}%
    \end{singlespace}
}%

% Chapter Intro Para Formatting
\newcommand{\intropara}[1]{%
    {%
        \fontsize{11.5}{0}%
        \selectfont%
        \textbf{#1}%
    }%
}%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Figures and Tables %%%%%%%%%%%%%%%%%%%%%%%%%%%%

% New commands to manually adjust table widths
\newcolumntype{L}[1]{>{\raggedright\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{C}[1]{>{\centering\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}
\newcolumntype{R}[1]{>{\raggedleft\let\newline\\\arraybackslash\hspace{0pt}}m{#1}}

% setting the column space to 0
\setlength{\tabcolsep}{0pt}

% Source/Notes Formatting
\newcommand{\source}[1]{%
    {%
        \fontsize{8}{0}%
        \selectfont%
        \noindent%
        #1%
    }%
}%

% Figure Numbering
\renewcommand{\fnum@figure}{
    {%
        \fontsize{9}{0}%
        \selectfont%
        \noindent%
        \textcolor{urban-blue}{\MakeUppercase{Figure\thefigure\newline}}%
    }%
}

% Table Numbering
\renewcommand{\fnum@table}{
    {%
        \fontsize{9}{0}%
        \selectfont%
        \noindent%
        \textcolor{urban-blue}{\MakeUppercase{Table~\thetable\\}}%
    }%
}

% Heading Title
\newcommand{\tabheader}[1]{%
    {%
        \fontsize{9.5}{0}%
        \fontseries{b}%
        \selectfont%
        \noindent%
        #1%
    }%
}%

% Row Heading Title
\newcommand{\rowheader}[1]{%
    {%
        \fontsize{9}{0}%
        \fontseries{b}%
        \selectfont%
        \noindent%
        #1%
    }%
}%

% Row Subheading Title
\newcommand{\rowsubheader}[1]{%
    {%
        \fontsize{9}{0}%
        \fontshape{it}%
        \selectfont%
        \noindent%
        #1%
    }%
}%

% Table Font
\newcommand{\tabfont}[1]{%
    {%
        \fontsize{9}{0}%
        \selectfont%
        \noindent%
        #1%
    }%
}%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Notes and References %%%%%%%%%%%%%%%%%%%%%%%%%%
% Endnote font size
\renewcommand\enotesize{\fontsize{9}{9}\selectfont}

% Mark with no indent and hanging indent
\renewcommand\enoteformat{%
   \noindent$^\theenmark$\hspace{0.05in}\ \hangindent 0.55\parindent%
}

% Reference font formatting
\renewcommand*{\bibfont}{\fontsize{9}{9}\selectfont}

% Hanging indent
\setlength{\bibhang}{0.15in}

% Changing et al
\DefineBibliographyStrings{english}{andothers={and colleagues}}

\title{Title}