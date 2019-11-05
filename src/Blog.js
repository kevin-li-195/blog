import React, { useState } from 'react';
import { BrowserRouter, Route, Link } from 'react-router-dom';
import './App.scss';
import marked from 'marked-katex';
import ReactMarkdown from 'react-markdown';
import MathJax from 'react-mathjax';
import RemarkMathPlugin from 'remark-math';

function MarkdownRender(props) {
    const newProps = {
        ...props,
        plugins: [
          RemarkMathPlugin,
        ],
        renderers: {
          ...props.renderers,
          math: (props) =>
            <MathJax.Node formula={props.value} />,
          inlineMath: (props) =>
            <MathJax.Node inline formula={props.value} />
        }
      };
      return (
        <MathJax.Provider input="tex">
            <ReactMarkdown {...newProps} />
        </MathJax.Provider>
      );
}

marked.setOptions({
  renderer: new marked.Renderer(),
  gfm: true,
  tables: true,
  breaks: false,
  pedantic: false,
  sanitize: false,
  smartLists: true,
  smartypants: false
});

interface RenderableFile {
    filename: string,
    label: string,
    path: string
}

interface MarkdownProps {
    post: RenderableFile
}

interface ChartProps {
    chart: RenderableFile
}

export const Markdown: React.FC<MarkdownProps> = ({post}) => {
    const [postContents, setPostContents] = useState(null);

    fetch(`/markdown_posts/${post.filename}`)
        .then(resp => resp.text())
        .then(text => {
            console.log(text);
            setPostContents(text)
        });
    
    return(
        postContents === null
        ? <></>
        // : <article dangerouslySetInnerHTML={{ __html : postContents }}/>
        : <MarkdownRender source={postContents} />
    )
}

export const Chart: React.FC<ChartProps> = ({chart}) => {
    return(
        <iframe title={chart.label} src={`/chart_pages/${chart.filename}`} />
    )
}

const blogposts: Post[] = [
    {
        filename: "beta-over-time.md",
        label: "Beta over Time",
        path: "beta-over-time"
    },
    {
        filename: "2017-01-31-snek-lizard-paradox.md",
        label: "The Snake-Lizard Probability Paradox",
        path: "snake-lizard"
    },
    {
        filename: "2016-08-24-sampling-monad.md",
        label: "Sampling Monad (reinventing the wheel)",
        path: "sampling-monad"
    }
];

const charts = [
    {
        filename: "yield-curve.html",
        label: "3D Yield Curve",
        path: "yield-curve-chart"
    }
]

const Header: React.FC<void> = () => (
    <header>
        <h1>Blog - Kevin Li</h1>
        <Link to="/posts">Posts</Link>
        <Link to="/charts">Charts</Link>
    </header>
)

export const Blog: React.FC<void> = () => {
    return(
        <div className="container">
            <div className="container-item">
            <BrowserRouter>
            <Header />
                <Route path={"/posts"} render={() => {
                    return(
                        <header>
                            {
                                blogposts.map((post) => {
                                    return(
                                        <Link key={post.path} to={`/posts/${post.path}`}>
                                            {post.label}
                                        </Link>
                                    )
                                })
                            }
                        </header>
                    )
                }}/>
                <Route path={"/charts"} render={() => {
                    return(
                        <header>
                            {
                                charts.map((chart) => {
                                    return(
                                        <Link key={chart.path} to={`/charts/${chart.path}`}>
                                            {chart.label}
                                        </Link>
                                    )
                                })
                            }
                        </header>
                    )
                }}/>
                <Route exact path={["/", "/posts", "/charts"]} render={() => {
                    return(
                        <div>
                        <p>
                            Hey, welcome to my blog. I
                            sometimes write stuff here that I
                            find interesting or educational.
                            <br/>
                            <br/>
                            Broadly, the subjects span economics,
                            finance, probability & stats, and programming.
                            <br/>
                            <br/>
                            Enjoy your stay!
                            <br/>
                            <br/>
                            You can email me at: zl2606 [at(@)] columbia [dot(.)] edu
                            if you'd like to chat!
                        </p>
                        </div>
                    )
                }} />
                {
                    blogposts.map((post) => {
                        return(
                            <Route path={`/posts/${post.path}`} render={() => 
                                <Markdown post={post} />
                            } />
                        )
                    }
                    )
                }
                {
                    charts.map((chart) => {
                        return(
                            <Route path={`/charts/${chart.path}`} render={() => 
                                <Chart chart={chart} />
                            } />
                        )
                    }
                    )
                }
            </BrowserRouter>
            </div>
        </div>
    )
};
