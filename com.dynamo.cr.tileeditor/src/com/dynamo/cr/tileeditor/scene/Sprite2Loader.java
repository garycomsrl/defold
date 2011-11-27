package com.dynamo.cr.tileeditor.scene;

import java.io.IOException;
import java.io.InputStream;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

import com.dynamo.cr.sceneed.core.ISceneView.ILoaderContext;
import com.dynamo.cr.sceneed.core.ISceneView.INodeLoader;
import com.google.protobuf.Message;

public class Sprite2Loader implements INodeLoader<Sprite2Node> {

    @Override
    public Sprite2Node load(ILoaderContext context, InputStream contents)
            throws IOException, CoreException {
        return new Sprite2Node();
    }

    @Override
    public Message buildMessage(ILoaderContext context, Sprite2Node node,
            IProgressMonitor monitor) throws IOException, CoreException {
        return null;
    }
}
