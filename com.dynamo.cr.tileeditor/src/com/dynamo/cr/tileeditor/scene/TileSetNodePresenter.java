package com.dynamo.cr.tileeditor.scene;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;

import com.dynamo.cr.sceneed.core.ISceneView.INodePresenter;
import com.dynamo.cr.sceneed.core.ISceneView.IPresenterContext;
import com.dynamo.cr.sceneed.core.Node;
import com.dynamo.cr.tileeditor.operations.AddCollisionGroupNodeOperation;
import com.dynamo.cr.tileeditor.operations.RemoveCollisionGroupNodeOperation;
import com.dynamo.cr.tileeditor.operations.SetTileCollisionGroupsOperation;

public class TileSetNodePresenter implements INodePresenter<TileSetNode> {
    // Used for painting collision groups onto tiles (convex hulls)
    // TODO remove this state, should be handled by a painter tool with state
    private List<String> oldTileCollisionGroups;
    private List<String> newTileCollisionGroups;
    private String currentCollisionGroup;

    public void onBeginPaintTile(IPresenterContext presenterContext) {
        CollisionGroupNode collisionGroup = getCollisionGroup(presenterContext);
        TileSetNode tileSet = collisionGroup.getTileSetNode();
        this.oldTileCollisionGroups = tileSet.getTileCollisionGroups();
        this.newTileCollisionGroups = new ArrayList<String>(this.oldTileCollisionGroups);
        this.currentCollisionGroup = collisionGroup.getName();
    }

    public void onEndPaintTile(IPresenterContext presenterContext) {
        if (this.currentCollisionGroup != null) {
            CollisionGroupNode collisionGroup = getCollisionGroup(presenterContext);
            TileSetNode tileSet = collisionGroup.getTileSetNode();
            if (!this.oldTileCollisionGroups.equals(this.newTileCollisionGroups)) {
                presenterContext.executeOperation(new SetTileCollisionGroupsOperation(
                        tileSet, this.oldTileCollisionGroups,
                        this.newTileCollisionGroups,
                        this.currentCollisionGroup));
            }
            this.currentCollisionGroup = null;
        }
    }

    public void onPaintTile(IPresenterContext presenterContext, int index) {
        if (this.currentCollisionGroup != null) {
            CollisionGroupNode collisionGroup = getCollisionGroup(presenterContext);
            TileSetNode tileSet = collisionGroup.getTileSetNode();
            if (!this.newTileCollisionGroups.get(index).equals(this.currentCollisionGroup)) {
                this.newTileCollisionGroups.set(index, this.currentCollisionGroup);
                tileSet.setTileCollisionGroups(this.newTileCollisionGroups);
                presenterContext.refreshView();
            }
        }
    }

    public void onAddCollisionGroup(IPresenterContext presenterContext) {
        TileSetNode tileSet = getTileSet(presenterContext);
        presenterContext.executeOperation(new AddCollisionGroupNodeOperation(tileSet, new CollisionGroupNode(), presenterContext));
    }

    public void onRemoveCollisionGroup(IPresenterContext presenterContext) {
        CollisionGroupNode collisionGroup = getCollisionGroup(presenterContext);
        presenterContext.executeOperation(new RemoveCollisionGroupNodeOperation(collisionGroup, presenterContext));
    }

    public void onSelectCollisionGroup(IPresenterContext presenterContext, int index) {
        TileSetNode tileSet = getTileSet(presenterContext);
        if (tileSet != null) {
            Node selected = tileSet;
            List<Node> children = tileSet.getChildren();
            if (index >= 0 && index < children.size()) {
                selected = children.get(index);
            }
            IStructuredSelection selection = new StructuredSelection(selected);
            if (!selection.equals(presenterContext.getSelection())) {
                presenterContext.setSelection(selection);
            }
        }
    }

    private CollisionGroupNode getCollisionGroup(IPresenterContext presenterContext) {
        return (CollisionGroupNode) presenterContext.getSelection().getFirstElement();
    }

    private TileSetNode getTileSet(IPresenterContext presenterContext) {
        Node node = (Node) presenterContext.getSelection().getFirstElement();
        if (node instanceof TileSetNode) {
            return (TileSetNode) node;
        } else if (node instanceof CollisionGroupNode) {
            return (TileSetNode) node.getParent();
        }
        return null;
    }
}
