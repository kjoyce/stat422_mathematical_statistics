% Class for formatting Homework
% Kevin Joyce 2011
%
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{stat_homework}[2011/02/10 Homework Class file by Kevin Joyce]
\RequirePackage{etoolbox}
\LoadClassWithOptions{article}
\RequirePackage{  
  amsmath,
  amscd,
  amssymb,
  amsthm,
  latexsym,
  graphicx,
  ulem,
  enumerate,
  float,
  ulsy,
%  subfigure,
  wrapfig,
  ifthen,
  rotating,
  url,
  mdwlist,
  fancyhdr,
}
\RequirePackage[margin=1in]{geometry}
\RequirePackage{cmacros}
\RequirePackage[mathcal]{euscript}

\pagestyle{fancy}
\setlength{\parindent}{0em}
\setlength{\parskip}{.5em}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\renewcommand{\normalsize}{\fontsize{12pt}{13pt}\selectfont}

\newcounter{pnum}
\newcommand*{\problem}[1]{
  \nopagebreak[4]
  \setcounter{opnum}{0}
  \setcounter{spnum}{0}
  \stepcounter{pnum}
  \textbf{\arabic{pnum}.} 
  \nopagebreak[4] #1 
}
\newcommand*{\mcproblem}[2]{
  \begin{samepage}
  \problem{#1} \par
  \vspace{.5cm} % Place options
  \nopagebreak[4] 
  #2
  \vspace{.5cm}
  \par
  \end{samepage}
}
\newenvironment{longproblem} {
    \setcounter{opnum}{0}
    \setcounter{spnum}{0}
    \stepcounter{pnum}
    \textbf{\arabic{pnum}.} 
  } {
    \par
  }
\newenvironment{longsubproblem} {
  \stepcounter{spnum}
  \hspace{1cm} \textbf{(\alph{spnum})} 
  } 
  {
    \vspace{.5cm}
  }
\newcommand*{\multproblem}[2]{
  \problem{#1} \par
  \vspace{.2cm}
  #2
  \vspace{.5cm}
}

\newcounter{spnum}
\def \subproblem #1{
  \subproblemm{#1} \par
}
\def \subproblemm #1{
  \nopagebreak[4]
  \stepcounter{spnum}
  \hangindent=1cm 
  \hangafter=0
  \textbf{(\alph{spnum})} #1 
}

\newcounter{opnum}
\def\option #1{
 \optionn #1 \par
}
\def \optionn {
  \nopagebreak[4]
  \stepcounter{opnum}
  (\alph{opnum})
}
\newenvironment{multchoice} { 
    \part{Multiple Choice}
  }
  {
    \par \underline{\hspace{\textwidth}}
  }


\newenvironment{shortanswer} {
  \part{Short Answer}
} {
}

\newenvironment{freeresponse} {
  \part{Free Response}

} {
}

\newenvironment{truefalsecx} 
{
  \part{True or False.}
  For each statement determine if it is true or false.  If it is true, just say so, if it is false give a counter example.\par
}
{ 
  \vfill
  \hrule
  \newpage
}

\newenvironment{truefalse} 
{
  \part{True or False.}
  For each statement determine if the given statement is true or false.  You do not need to justify your answer.\par
}
{
  \vfill
  \hrule
  \newpage
}

\newcommand{\tfcxproblem}[1]{
  \tfproblem{ #1 }
  \vspace{3cm}
}

\newcommand{\tfproblem}[1]{
  \problem{ 
    \begin{minipage}{.15\textwidth}
      \quad  \textbf{T} \quad \textbf{F} 
    \end{minipage} 
    \begin{minipage}{.85\textwidth}
      #1 
    \end{minipage}
  }\par
  \hangindent7em
  \vspace{.6cm}
}
\newcommand{\Hint}{ {\it Hint:} }

\renewcommand{\title}{\lhead } 
\newcommand{\course}{\chead }
\renewcommand{\docdate}{\rhead }

\newcommand{\makename}{Name:\underline{\hspace{8cm}}\par}

%%%%%%%%%%%%%%%% solution version 2 %%%%%%%%%%%%%%%%%%%%%
% Until I figure this out, I am leaving it alone
%
%\RequirePackage{comment}
%\AtBeginDocument{\excludecomment{solution}}
%% This sets the @showsolutions variable
%\newcommand{\showsolutions}[1]{
%  \newcommand{\@showsolutions}{#1} % set @showsolutions variable
%  \includecomment{solution}
%}
%
%\specialcomment{solution}
%  {
%      \par {\bf Solution: }{}
%      \begin{quote}
%      \setlength{\parindent}{2em}
%      \par\mbox{\,}
%  }
%  { 
%      \end{quote}\qed 
%      \setlength{\parindent}{0em}
%  }
%%%%%%%%%%%%%%%%%% solution version 1 %%%%%%%%%%%%%%%%%%%%%%%%
%% OLD implementation of solution... things where I reference
%% solution parts will break upon compilation... oh well
%% fix that bridge when you get there
\newenvironment{solution}[1][null]
  {
      \par \textbf{Solution: }
	\ifthenelse{\NOT\equal{#1}{null}}{to part #1 }{}
      \begin{quote}
      \setlength{\parindent}{2em}
      \par\mbox{\,}
  }
  { 
      \end{quote}\qed 
      \setlength{\parindent}{0em}
  }

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\newenvironment{subproblems}
  { \begin{enumerate}[(a)] 
    \def\subprob{ \item }
  }
  { \end{enumerate} }

\newcounter{ppnum}
\newenvironment{problems}
  { \setcounter{ppnum}{0}
    \def\hproblem{
      \refstepcounter{ppnum}
      \par{\bf Problem \arabic{ppnum}.}
    }
    \def\hproblemsh{
      \vspace{2cm}
      \hproblem 
    }
  }
  {}
\newenvironment{aproblems}
  {\begin{problems}}
  {\end{problems}}

%\newcommand{\Hint}{ {\it Hint:} }
\newcommand{\pref}[1]{ {\bf Problem \ref{#1}}}

\renewcommand{\subproblem}[1] {  
  \nopagebreak[4]
  \stepcounter{spnum}
  \hangindent=1cm 
  \hangafter=0
  \textbf{(\alph{spnum})} {\ifdef{\@showsolutions}{\it}{} #1 \par }
}

\ProcessOptions\relax
