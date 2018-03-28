


/* A1 constants for Band HF1 */
#define MAX_HF1_A1_CONSTANTS 173

static float A1HF1[8][MAX_HF1_A1_CONSTANTS];

static float A1hf1tmp[MAX_HF1_A1_CONSTANTS][8]={  
/*   30db int. attenuator off        30db int. attenuator on   */   
/* +Ex     -Ex    +/-Ex     Ez     +Ex     -Ex    +/-Ex     Ez */   
{ 168.78f, 165.78f, 167.27f, 168.21f, 144.70f, 141.78f, 143.23f, 144.24f }, /* f0  0 */
{ 168.78f, 165.78f, 167.27f, 168.21f, 144.70f, 141.78f, 143.23f, 144.24f }, /* f1 25 */
{ 167.38f, 165.72f, 166.55f, 166.27f, 142.08f, 140.43f, 141.25f, 141.06f }, /* f2 50 */
{ 165.99f, 165.66f, 165.82f, 164.33f, 139.46f, 139.08f, 139.27f, 137.87f }, /* f4 75 */
{ 169.41f, 169.07f, 169.24f, 167.95f, 142.92f, 142.58f, 142.75f, 141.54f }, 
{ 172.83f, 172.48f, 172.65f, 171.58f, 146.38f, 146.09f, 146.23f, 145.20f },
{ 174.14f, 173.78f, 173.96f, 173.13f, 147.60f, 147.29f, 147.44f, 146.69f },
{ 175.46f, 175.08f, 175.27f, 174.68f, 148.83f, 148.48f, 148.65f, 148.18f },
{ 175.98f, 175.63f, 175.80f, 175.37f, 149.38f, 149.00f, 149.19f, 148.89f },
{ 176.50f, 176.17f, 176.34f, 176.06f, 149.94f, 149.53f, 149.73f, 149.60f },
{ 176.74f, 176.38f, 176.56f, 176.38f, 150.17f, 149.79f, 149.98f, 149.96f },
{ 176.97f, 176.59f, 176.78f, 176.70f, 150.40f, 150.05f, 150.23f, 150.32f },
{ 177.05f, 176.75f, 176.90f, 176.88f, 150.57f, 150.21f, 150.39f, 150.52f },
{ 177.14f, 176.92f, 177.03f, 177.06f, 150.73f, 150.38f, 150.55f, 150.72f },
{ 177.23f, 176.96f, 177.09f, 177.12f, 150.82f, 150.49f, 150.65f, 150.83f },
{ 177.33f, 177.00f, 177.16f, 177.18f, 150.90f, 150.61f, 150.76f, 150.93f },
{ 177.36f, 177.01f, 177.18f, 177.30f, 150.93f, 150.65f, 150.79f, 151.00f },
{ 177.38f, 177.03f, 177.21f, 177.42f, 150.96f, 150.70f, 150.83f, 151.07f },
{ 177.41f, 177.05f, 177.23f, 177.45f, 151.04f, 150.70f, 150.87f, 151.13f },
{ 177.44f, 177.08f, 177.26f, 177.48f, 151.11f, 150.70f, 150.90f, 151.20f },
{ 177.43f, 177.12f, 177.27f, 177.49f, 151.11f, 150.71f, 150.91f, 151.21f },
{ 177.41f, 177.16f, 177.29f, 177.51f, 151.11f, 150.73f, 150.92f, 151.22f },
{ 177.43f, 177.23f, 177.33f, 177.56f, 151.10f, 150.77f, 150.93f, 151.26f },
{ 177.44f, 177.30f, 177.37f, 177.62f, 151.08f, 150.82f, 150.95f, 151.31f },
{ 177.52f, 177.30f, 177.41f, 177.64f, 151.10f, 150.88f, 150.99f, 151.36f },
{ 177.61f, 177.30f, 177.45f, 177.66f, 151.11f, 150.94f, 151.02f, 151.41f },
{ 177.52f, 177.32f, 177.42f, 177.67f, 151.14f, 150.89f, 151.02f, 151.44f },
{ 177.44f, 177.33f, 177.39f, 177.68f, 151.17f, 150.85f, 151.01f, 151.47f },
{ 177.51f, 177.26f, 177.39f, 177.66f, 151.14f, 150.92f, 151.03f, 151.46f },
{ 177.58f, 177.19f, 177.39f, 177.64f, 151.11f, 151.00f, 151.05f, 151.44f },
{ 177.57f, 177.26f, 177.42f, 177.66f, 151.14f, 150.89f, 151.02f, 151.46f },
{ 177.55f, 177.33f, 177.44f, 177.68f, 151.17f, 150.79f, 150.98f, 151.47f },
{ 177.55f, 177.25f, 177.40f, 177.67f, 151.13f, 150.85f, 150.99f, 151.47f },
{ 177.55f, 177.17f, 177.36f, 177.66f, 151.08f, 150.91f, 151.00f, 151.48f },
{ 177.53f, 177.24f, 177.38f, 177.67f, 151.09f, 150.92f, 151.00f, 151.43f },
{ 177.50f, 177.31f, 177.40f, 177.67f, 151.09f, 150.94f, 151.01f, 151.39f },
{ 177.47f, 177.25f, 177.36f, 177.64f, 151.12f, 150.91f, 151.01f, 151.42f },
{ 177.45f, 177.20f, 177.32f, 177.61f, 151.14f, 150.88f, 151.01f, 151.45f },
{ 177.47f, 177.21f, 177.34f, 177.62f, 151.13f, 150.94f, 151.04f, 151.45f },
{ 177.50f, 177.23f, 177.36f, 177.63f, 151.12f, 151.00f, 151.06f, 151.45f },
{ 177.45f, 177.18f, 177.32f, 177.60f, 151.10f, 150.94f, 151.02f, 151.41f },
{ 177.39f, 177.14f, 177.27f, 177.57f, 151.09f, 150.88f, 150.99f, 151.37f },
{ 177.39f, 177.14f, 177.27f, 177.56f, 151.09f, 150.87f, 150.98f, 151.41f },
{ 177.39f, 177.14f, 177.27f, 177.56f, 151.09f, 150.85f, 150.97f, 151.45f },
{ 177.39f, 177.09f, 177.24f, 177.53f, 151.07f, 150.85f, 150.96f, 151.37f },
{ 177.39f, 177.04f, 177.21f, 177.50f, 151.06f, 150.85f, 150.96f, 151.30f },
{ 177.37f, 177.02f, 177.19f, 177.47f, 151.05f, 150.83f, 150.94f, 151.29f },
{ 177.34f, 177.01f, 177.17f, 177.45f, 151.03f, 150.80f, 150.91f, 151.29f },
{ 177.34f, 177.01f, 177.17f, 177.46f, 151.00f, 150.80f, 150.90f, 151.26f },
{ 177.34f, 177.01f, 177.18f, 177.48f, 150.97f, 150.80f, 150.89f, 151.24f },
{ 177.33f, 177.02f, 177.18f, 177.46f, 150.99f, 150.78f, 150.89f, 151.26f },
{ 177.31f, 177.04f, 177.18f, 177.45f, 151.00f, 150.77f, 150.89f, 151.27f },
{ 177.34f, 177.03f, 177.18f, 177.45f, 151.03f, 150.77f, 150.90f, 151.27f },
{ 177.37f, 177.01f, 177.19f, 177.45f, 151.06f, 150.77f, 150.92f, 151.26f },
{ 177.30f, 177.01f, 177.16f, 177.43f, 150.99f, 150.76f, 150.87f, 151.22f },
{ 177.23f, 177.01f, 177.12f, 177.41f, 150.92f, 150.74f, 150.83f, 151.19f },
{ 177.29f, 177.01f, 177.15f, 177.42f, 150.96f, 150.74f, 150.85f, 151.17f },
{ 177.35f, 177.02f, 177.18f, 177.42f, 151.01f, 150.74f, 150.88f, 151.16f },
{ 177.25f, 176.96f, 177.11f, 177.34f, 150.99f, 150.73f, 150.86f, 151.14f },
{ 177.15f, 176.91f, 177.03f, 177.26f, 150.98f, 150.72f, 150.85f, 151.13f },
{ 177.11f, 176.95f, 177.03f, 177.28f, 150.89f, 150.72f, 150.81f, 151.13f },
{ 177.07f, 176.99f, 177.03f, 177.30f, 150.81f, 150.72f, 150.76f, 151.13f },
{ 177.07f, 176.95f, 177.01f, 177.25f, 150.79f, 150.72f, 150.76f, 151.09f },
{ 177.07f, 176.91f, 176.99f, 177.20f, 150.78f, 150.72f, 150.75f, 151.06f },
{ 177.03f, 176.85f, 176.94f, 177.18f, 150.78f, 150.69f, 150.73f, 151.07f },
{ 176.99f, 176.80f, 176.90f, 177.15f, 150.78f, 150.66f, 150.72f, 151.09f },
{ 177.01f, 176.79f, 176.90f, 177.14f, 150.79f, 150.69f, 150.74f, 151.06f },
{ 177.02f, 176.77f, 176.90f, 177.14f, 150.81f, 150.72f, 150.77f, 151.03f },
{ 176.97f, 176.73f, 176.85f, 177.11f, 150.78f, 150.66f, 150.72f, 150.98f },
{ 176.91f, 176.69f, 176.80f, 177.08f, 150.75f, 150.61f, 150.68f, 150.93f },
{ 176.97f, 176.67f, 176.82f, 177.10f, 150.74f, 150.59f, 150.67f, 150.91f },
{ 177.03f, 176.64f, 176.83f, 177.11f, 150.73f, 150.58f, 150.65f, 150.90f },
{ 176.97f, 176.64f, 176.81f, 177.06f, 150.71f, 150.54f, 150.62f, 150.87f },
{ 176.92f, 176.64f, 176.78f, 177.01f, 150.70f, 150.49f, 150.59f, 150.84f },
{ 176.86f, 176.64f, 176.75f, 176.99f, 150.70f, 150.48f, 150.59f, 150.82f },
{ 176.81f, 176.64f, 176.73f, 176.97f, 150.70f, 150.46f, 150.58f, 150.81f },
{ 176.78f, 176.63f, 176.71f, 176.94f, 150.66f, 150.44f, 150.55f, 150.78f },
{ 176.75f, 176.62f, 176.69f, 176.90f, 150.61f, 150.41f, 150.51f, 150.75f },
{ 176.73f, 176.62f, 176.67f, 176.86f, 150.63f, 150.47f, 150.55f, 150.78f },
{ 176.70f, 176.62f, 176.66f, 176.81f, 150.64f, 150.53f, 150.59f, 150.80f },
{ 176.72f, 176.57f, 176.64f, 176.80f, 150.63f, 150.47f, 150.55f, 150.78f },
{ 176.73f, 176.51f, 176.62f, 176.78f, 150.62f, 150.41f, 150.51f, 150.76f },
{ 176.69f, 176.47f, 176.58f, 176.78f, 150.50f, 150.41f, 150.46f, 150.70f },
{ 176.65f, 176.43f, 176.54f, 176.77f, 150.38f, 150.41f, 150.40f, 150.65f },
{ 176.65f, 176.42f, 176.53f, 176.76f, 150.43f, 150.38f, 150.41f, 150.67f },
{ 176.65f, 176.41f, 176.53f, 176.74f, 150.47f, 150.36f, 150.41f, 150.68f },
{ 176.63f, 176.36f, 176.50f, 176.70f, 150.46f, 150.34f, 150.40f, 150.63f },
{ 176.60f, 176.32f, 176.46f, 176.66f, 150.45f, 150.33f, 150.39f, 150.58f },
{ 176.61f, 176.33f, 176.47f, 176.67f, 150.42f, 150.34f, 150.38f, 150.59f },
{ 176.63f, 176.33f, 176.48f, 176.68f, 150.39f, 150.36f, 150.37f, 150.60f },
{ 176.57f, 176.30f, 176.44f, 176.65f, 150.39f, 150.36f, 150.38f, 150.56f },
{ 176.52f, 176.27f, 176.40f, 176.62f, 150.39f, 150.36f, 150.38f, 150.51f },
{ 176.48f, 176.26f, 176.37f, 176.57f, 150.35f, 150.32f, 150.33f, 150.47f },
{ 176.44f, 176.25f, 176.34f, 176.51f, 150.31f, 150.28f, 150.29f, 150.44f },
{ 176.39f, 176.25f, 176.32f, 176.51f, 150.34f, 150.28f, 150.31f, 150.47f },
{ 176.33f, 176.25f, 176.29f, 176.52f, 150.37f, 150.28f, 150.32f, 150.50f },
{ 176.31f, 176.21f, 176.26f, 176.47f, 150.37f, 150.27f, 150.32f, 150.46f },
{ 176.28f, 176.17f, 176.23f, 176.43f, 150.37f, 150.25f, 150.31f, 150.43f },
{ 176.28f, 176.16f, 176.22f, 176.41f, 150.30f, 150.28f, 150.29f, 150.39f },
{ 176.28f, 176.15f, 176.21f, 176.39f, 150.22f, 150.31f, 150.27f, 150.36f },
{ 176.28f, 176.08f, 176.18f, 176.38f, 150.24f, 150.33f, 150.28f, 150.36f },
{ 176.28f, 176.01f, 176.15f, 176.37f, 150.26f, 150.34f, 150.30f, 150.37f },
{ 176.29f, 175.98f, 176.13f, 176.35f, 150.23f, 150.29f, 150.26f, 150.34f },
{ 176.29f, 175.96f, 176.12f, 176.34f, 150.20f, 150.23f, 150.21f, 150.32f },
{ 176.27f, 175.96f, 176.12f, 176.34f, 150.24f, 150.22f, 150.23f, 150.33f },
{ 176.26f, 175.96f, 176.11f, 176.34f, 150.29f, 150.20f, 150.25f, 150.35f },
{ 176.19f, 175.96f, 176.08f, 176.29f, 150.22f, 150.17f, 150.20f, 150.30f },
{ 176.13f, 175.96f, 176.04f, 176.23f, 150.15f, 150.15f, 150.15f, 150.25f },
{ 176.09f, 175.94f, 176.01f, 176.22f, 150.13f, 150.13f, 150.13f, 150.22f },
{ 176.05f, 175.91f, 175.98f, 176.20f, 150.12f, 150.12f, 150.12f, 150.20f },
{ 176.02f, 175.93f, 175.97f, 176.18f, 150.09f, 150.11f, 150.10f, 150.20f },
{ 175.99f, 175.94f, 175.97f, 176.17f, 150.06f, 150.09f, 150.08f, 150.19f },
{ 175.96f, 175.90f, 175.93f, 176.12f, 150.05f, 150.06f, 150.06f, 150.15f },
{ 175.92f, 175.86f, 175.89f, 176.06f, 150.04f, 150.04f, 150.04f, 150.12f },
{ 175.93f, 175.82f, 175.88f, 176.05f, 150.02f, 150.02f, 150.02f, 150.10f },
{ 175.95f, 175.78f, 175.86f, 176.05f, 150.01f, 150.01f, 150.01f, 150.09f },
{ 175.93f, 175.78f, 175.86f, 176.03f, 150.00f, 150.01f, 150.00f, 150.08f },
{ 175.92f, 175.78f, 175.85f, 176.01f, 149.98f, 150.01f, 150.00f, 150.06f },
{ 175.91f, 175.81f, 175.86f, 176.00f, 150.00f, 150.01f, 150.01f, 150.05f },
{ 175.90f, 175.84f, 175.87f, 175.98f, 150.01f, 150.01f, 150.01f, 150.04f },
{ 175.87f, 175.75f, 175.81f, 175.96f, 150.00f, 150.02f, 150.01f, 150.03f },
{ 175.84f, 175.65f, 175.75f, 175.93f, 149.99f, 150.02f, 150.00f, 150.02f },
{ 175.79f, 175.67f, 175.73f, 175.91f, 150.00f, 150.00f, 150.00f, 150.00f },
{ 175.74f, 175.68f, 175.71f, 175.89f, 150.02f, 149.99f, 150.01f, 149.98f },
{ 175.74f, 175.64f, 175.69f, 175.89f, 150.01f, 149.98f, 149.99f, 149.98f },
{ 175.74f, 175.60f, 175.67f, 175.89f, 149.99f, 149.96f, 149.98f, 149.97f },
{ 175.73f, 175.58f, 175.65f, 175.87f, 149.94f, 149.95f, 149.94f, 149.93f },
{ 175.72f, 175.55f, 175.63f, 175.84f, 149.88f, 149.94f, 149.91f, 149.88f },
{ 175.66f, 175.55f, 175.61f, 175.82f, 149.90f, 149.97f, 149.93f, 149.91f },
{ 175.61f, 175.55f, 175.58f, 175.80f, 149.91f, 150.00f, 149.96f, 149.93f },
{ 175.58f, 175.53f, 175.56f, 175.76f, 149.88f, 149.96f, 149.92f, 149.89f },
{ 175.56f, 175.50f, 175.53f, 175.72f, 149.86f, 149.91f, 149.89f, 149.84f },
{ 175.56f, 175.52f, 175.54f, 175.71f, 149.81f, 149.90f, 149.86f, 149.83f },
{ 175.56f, 175.53f, 175.55f, 175.70f, 149.77f, 149.89f, 149.83f, 149.82f },
{ 175.56f, 175.45f, 175.51f, 175.67f, 149.77f, 149.92f, 149.85f, 149.82f },
{ 175.56f, 175.37f, 175.47f, 175.64f, 149.77f, 149.95f, 149.86f, 149.82f },
{ 175.54f, 175.40f, 175.47f, 175.62f, 149.75f, 149.95f, 149.85f, 149.82f },
{ 175.51f, 175.43f, 175.47f, 175.61f, 149.72f, 149.95f, 149.84f, 149.82f },
{ 175.51f, 175.35f, 175.43f, 175.61f, 149.72f, 149.91f, 149.82f, 149.79f },
{ 175.51f, 175.27f, 175.39f, 175.61f, 149.72f, 149.87f, 149.80f, 149.75f },
{ 175.43f, 175.24f, 175.34f, 175.56f, 149.70f, 149.84f, 149.77f, 149.73f },
{ 175.35f, 175.22f, 175.29f, 175.51f, 149.67f, 149.81f, 149.74f, 149.71f },
{ 175.33f, 175.21f, 175.27f, 175.49f, 149.65f, 149.79f, 149.72f, 149.67f },
{ 175.30f, 175.19f, 175.25f, 175.48f, 149.64f, 149.76f, 149.70f, 149.62f },
{ 175.29f, 175.19f, 175.24f, 175.48f, 149.66f, 149.82f, 149.74f, 149.68f },
{ 175.28f, 175.20f, 175.24f, 175.48f, 149.67f, 149.88f, 149.78f, 149.73f },
{ 175.23f, 175.14f, 175.18f, 175.43f, 149.68f, 149.81f, 149.74f, 149.70f },
{ 175.17f, 175.09f, 175.13f, 175.38f, 149.68f, 149.74f, 149.71f, 149.66f },
{ 175.17f, 175.11f, 175.14f, 175.39f, 149.68f, 149.74f, 149.71f, 149.66f },
{ 175.18f, 175.12f, 175.15f, 175.39f, 149.68f, 149.74f, 149.71f, 149.66f },
{ 175.19f, 175.08f, 175.14f, 175.38f, 149.68f, 149.74f, 149.71f, 149.63f },
{ 175.21f, 175.04f, 175.12f, 175.36f, 149.68f, 149.74f, 149.71f, 149.59f },
{ 175.15f, 175.04f, 175.10f, 175.34f, 149.66f, 149.74f, 149.70f, 149.60f },
{ 175.10f, 175.05f, 175.07f, 175.32f, 149.63f, 149.75f, 149.69f, 149.60f },
{ 175.06f, 174.97f, 175.01f, 175.28f, 149.62f, 149.72f, 149.67f, 149.53f },
{ 175.02f, 174.89f, 174.95f, 175.24f, 149.60f, 149.69f, 149.65f, 149.47f },
{ 175.05f, 174.90f, 174.98f, 175.29f, 149.63f, 149.71f, 149.67f, 149.56f },
{ 175.08f, 174.92f, 175.00f, 175.33f, 149.67f, 149.72f, 149.69f, 149.66f },
{ 175.03f, 174.88f, 174.95f, 175.28f, 149.61f, 149.71f, 149.66f, 149.61f },
{ 174.97f, 174.84f, 174.91f, 175.24f, 149.55f, 149.70f, 149.62f, 149.55f },
{ 174.92f, 174.84f, 174.88f, 175.23f, 149.61f, 149.73f, 149.67f, 149.55f },
{ 174.87f, 174.84f, 174.86f, 175.21f, 149.67f, 149.76f, 149.72f, 149.55f },
{ 174.86f, 174.82f, 174.84f, 175.19f, 149.57f, 149.72f, 149.64f, 149.49f },
{ 174.85f, 174.79f, 174.82f, 175.16f, 149.47f, 149.68f, 149.57f, 149.44f },
{ 174.85f, 174.82f, 174.83f, 175.16f, 149.52f, 149.69f, 149.60f, 149.47f },
{ 174.85f, 174.85f, 174.85f, 175.15f, 149.56f, 149.71f, 149.64f, 149.50f },
{ 174.85f, 174.81f, 174.83f, 175.13f, 149.56f, 149.74f, 149.65f, 149.46f },
{ 174.85f, 174.77f, 174.81f, 175.11f, 149.57f, 149.77f, 149.67f, 149.43f },
{ 174.81f, 174.76f, 174.79f, 175.13f, 149.58f, 149.76f, 149.67f, 149.46f },
{ 174.78f, 174.75f, 174.76f, 175.14f, 149.60f, 149.75f, 149.67f, 149.49f },
{ 174.80f, 174.75f, 174.78f, 175.11f, 149.50f, 149.73f, 149.62f, 149.46f },
{ 174.83f, 174.75f, 174.79f, 175.08f, 149.40f, 149.72f, 149.56f, 149.42f },  /* f171 */
{ 174.81f, 174.75f, 174.78f, 175.08f, 149.45f, 149.72f, 149.58f, 149.42f }   /* f172 */
};

/* f171 - 4.275MHz */
/* f172 - 4.300MHz */