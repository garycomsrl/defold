#include <Box2D/Dynamics/Contacts/b2GridAndCircleContact.h>
#include <Box2D/Dynamics/b2Body.h>
#include <Box2D/Dynamics/b2Fixture.h>
#include <Box2D/Dynamics/b2WorldCallbacks.h>
#include <Box2D/Common/b2BlockAllocator.h>
#include <Box2D/Collision/b2TimeOfImpact.h>
#include <Box2D/Collision/Shapes/b2GridShape.h>
#include <Box2D/Collision/Shapes/b2PolygonShape.h>

#include <new>
using namespace std;

b2Contact* b2GridAndCircleContact::Create(b2Fixture* fixtureA, int32 indexA, b2Fixture* fixtureB, int32, b2BlockAllocator* allocator)
{
	void* mem = allocator->Allocate(sizeof(b2GridAndCircleContact));
	return new (mem) b2GridAndCircleContact(fixtureA, indexA, fixtureB);
}

void b2GridAndCircleContact::Destroy(b2Contact* contact, b2BlockAllocator* allocator)
{
	((b2GridAndCircleContact*)contact)->~b2GridAndCircleContact();
	allocator->Free(contact, sizeof(b2GridAndCircleContact));
}

b2GridAndCircleContact::b2GridAndCircleContact(b2Fixture* fixtureA, int32 indexA, b2Fixture* fixtureB)
	: b2Contact(fixtureA, indexA, fixtureB, 0)
{
	b2Assert(m_fixtureA->GetType() == b2Shape::e_grid);
	b2Assert(m_fixtureB->GetType() == b2Shape::e_circle);

    b2GridShape* gridShape = (b2GridShape*)m_fixtureA->GetShape();
    int32 row = m_indexA / gridShape->m_columnCount;
    int32 col = m_indexA - (gridShape->m_columnCount * row);
    m_edgeMask = gridShape->CalculateCellMask(m_fixtureA, row, col);
}

void b2GridAndCircleContact::Evaluate(b2Manifold* manifold, const b2Transform& xfA, const b2Transform& xfB)
{
    b2GridShape* gridShape = (b2GridShape*)m_fixtureA->GetShape();
    b2CircleShape* circleB = (b2CircleShape*)m_fixtureB->GetShape();

    const b2GridShape::Cell& cell = gridShape->m_cells[m_indexA];
    if (cell.m_Index == 0xffffffff)
    {
        return;
    }

    b2PolygonShape polyA;
    gridShape->GetPolygonShapeForCell(m_indexA, polyA);

    b2CollidePolygonAndCircle(manifold, &polyA, xfA, circleB, xfB);

    if (manifold->pointCount > 0)
    {
        // Remove points not present in edge-mask, ie internal and adjacent edges
        uint32 vc = polyA.m_vertexCount;
        for (uint32 i = 0; i < vc; ++i)
        {
            const b2Vec2 n = polyA.m_normals[i];
            float32 x = b2Dot(n, manifold->localNormal);
            // NOTE: constant epsilon is ok here as the magniute is constant
            if (x > 0.999f && !(m_edgeMask & (1 << i)))
            {
                manifold->pointCount = 0;
                break;
            }
        }
    }
}
