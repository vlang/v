using System;

internal readonly struct Vector
{
    public readonly double X;
    public readonly double Y;
    public readonly double Z;

    public Vector(double x, double y, double z)
    {
        X = x;
        Y = y;
        Z = z;
    }

    public override string ToString()
    {
        return $"({X}, {Y}, {Z})";
    }
}


class Program
{
    static void Main(string[] args)
    {
        const int boidsCount = 10000;

        var positions = new Vector[boidsCount];
        var velocities = new Vector[boidsCount];

        const double maxCoordinate = 10000.0;

        var random = new Random();

        for (var positionIndex = 0; positionIndex < positions.Length; positionIndex++)
        {
            positions[positionIndex] = new Vector(
                x: random.NextDouble() * maxCoordinate,
                y: random.NextDouble() * maxCoordinate,
                z: random.NextDouble() * maxCoordinate
            );
        }

        const double cohesionDistance = 10.0;
        const double separationDistance = 5.0;

        var closeBoids = new List<int>();

        for (var boidIndex = 0; boidIndex < positions.Length; boidIndex++)
        {
            var position = positions[boidIndex];
            closeBoids.Clear();

            for (var otherBoidIndex = 0; otherBoidIndex < positions.Length; otherBoidIndex++)
            {
                if (boidIndex == otherBoidIndex)
                {
                    continue;
                }

                var otherPosition = positions[otherBoidIndex];

                var differenceX = position.X - otherPosition.X;
                var differenceY = position.Y - otherPosition.Y;
                var differenceZ = position.Z - otherPosition.Z;

                var distance = differenceX * differenceX +
                    differenceY * differenceY +
                    differenceZ * differenceZ;

                if (distance <= cohesionDistance * cohesionDistance)
                {
                    closeBoids.Add(otherBoidIndex);
                }
            }

            if (closeBoids.Count == 0)
            {
                continue;
            }

            var cohesion = new Vector(0.0, 0.0, 0.0);
            var separation = new Vector(0.0, 0.0, 0.0);
            var separationCount = 0;
            var alignment = new Vector(0.0, 0.0, 0.0);

            foreach (var closeBoidIndex in closeBoids)
            {
                var closeBoidPosition = positions[closeBoidIndex];

                cohesion = new Vector(
                    cohesion.X + closeBoidPosition.X,
                    cohesion.Y + closeBoidPosition.Y,
                    cohesion.Z + closeBoidPosition.Z
                );

                var differenceFromClosest = new Vector(
                    position.X - closeBoidPosition.X,
                    position.Y - closeBoidPosition.Y,
                    position.Z - closeBoidPosition.Z
                );

                var differenceMagnitude = Math.Sqrt(differenceFromClosest.X * differenceFromClosest.X + differenceFromClosest.Y * differenceFromClosest.Y + differenceFromClosest.Z * differenceFromClosest.Z);

                if (differenceMagnitude <= separationDistance)
                {
                    separation = new Vector(
                        separation.X + differenceFromClosest.X / differenceMagnitude,
                        separation.Y + differenceFromClosest.Y / differenceMagnitude,
                        separation.Z + differenceFromClosest.Z / differenceMagnitude
                    );

                    separationCount++;
                }

                var closeBoidVelocity = velocities[closeBoidIndex];

                alignment = new Vector(
                    alignment.X + closeBoidVelocity.X,
                    alignment.Y + closeBoidVelocity.Y,
                    alignment.Z + closeBoidVelocity.Z
                );
            }

            cohesion = new Vector(
                cohesion.X / closeBoids.Count,
                cohesion.Y / closeBoids.Count,
                cohesion.Z / closeBoids.Count
            );

            var cohesionForce = new Vector(
                cohesion.X - position.X,
                cohesion.Y - position.Y,
                cohesion.Z - position.Z
            );

            if (separationCount > 0)
            {
                separation = new Vector(
                    separation.X / separationCount,
                    separation.Y / separationCount,
                    separation.Z / separationCount
                );
            }

            alignment = new Vector(
                alignment.X / closeBoids.Count,
                alignment.Y / closeBoids.Count,
                alignment.Z / closeBoids.Count
            );

            var currentVelocity = velocities[boidIndex];

            velocities[boidIndex] = new Vector(
                currentVelocity.X + cohesionForce.X + separation.X + alignment.X,
                currentVelocity.Y + cohesionForce.Y + separation.Y + alignment.Y,
                currentVelocity.Z + cohesionForce.Z + separation.Z + alignment.Z
            );
        }

        var positionSum = new Vector(0.0, 0.0, 0.0);
        var velocitySum = new Vector(0.0, 0.0, 0.0);

        for (var boidIndex = 0; boidIndex < positions.Length; boidIndex++)
        {
            var position = positions[boidIndex];
            var velocity = velocities[boidIndex];

            positions[boidIndex] = new Vector(
                position.X + velocity.X,
                position.Y + velocity.Y,
                position.Z + velocity.Z
            );

            positionSum = new Vector(
                positionSum.X + position.X,
                positionSum.Y + position.Y,
                positionSum.Z + position.Z
            );

            velocitySum = new Vector(
                velocitySum.X + velocity.X,
                velocitySum.Y + velocity.Y,
                velocitySum.Z + velocity.Z
            );
        }

        Console.WriteLine(positionSum.ToString());
        Console.WriteLine(velocitySum.ToString());
    }
}

