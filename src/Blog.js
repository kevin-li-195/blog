import React, { useState } from 'react';
import { BrowserRouter, Switch, Route, Link } from 'react-router-dom';
import './App.scss';
import marked from 'marked';

import markedImages from 'marked-images';

const renderer = new marked.Renderer();

markedImages(renderer);

interface MarkdownProps {
    post: Post
}

export const Markdown: React.FC<MarkdownProps> = ({post}) => {
    const [postContents, setPostContents] = useState(null);

    fetch(`/markdown_posts/${post.filename}`)
        .then(resp => resp.text())
        .then(text => {
            console.log(text);
            setPostContents(marked(text))
        });
    
    return(
        postContents === null
        ? <></>
        : <article dangerouslySetInnerHTML={{ __html : postContents }}/>
    )
}

interface Post {
    filename: string,
    label: string,
    path: string
}

const blogposts: Post[] = [
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

const Header: React.FC<void> = () => (
    <header className="header">
        <Link to="/posts">Posts</Link>
        <Link to="/charts">Charts</Link>
    </header>
)

const Charts: React.FC<void> = () => (
    <Switch>
    </Switch>
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
                {
                    blogposts.map((post) => 
                        <Route key={post.path + "post"} path={`/posts`} render={() => {
                            return(
                                <Route path={`/posts/${post.path}`} render={() => 
                                    <Markdown post={post} />
                                } />
                            )
                        }} />
                    )
                }
                <Route path="/charts" component={Charts} />
            </BrowserRouter>
            </div>
        </div>
    )
};
