/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.fail;

import java.net.ServerSocket;
import java.util.concurrent.Semaphore;

import javax.ws.rs.core.HttpHeaders;
import javax.ws.rs.core.MediaType;

import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.AfterTestExecutionCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.BeforeTestExecutionCallback;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.extension.ParameterResolver;
import org.testcontainers.containers.GenericContainer;
import org.testcontainers.containers.Network;
import org.testcontainers.containers.wait.strategy.Wait;
import org.testcontainers.utility.MountableFile;
import org.tinylog.Level;
import org.tinylog.provider.InternalLogger;

import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.Vertx;
import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.web.client.HttpResponse;
import io.vertx.ext.web.client.WebClient;
import io.vertx.ext.web.client.WebClientOptions;
import io.vertx.junit5.VertxExtension;

/**
 * Extension used to run integration tests over the WeNet interaction protocol
 * engine.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class WeNetInteractionProtocolEngineIntegrationExtension
		implements ParameterResolver, BeforeTestExecutionCallback, AfterTestExecutionCallback, BeforeEachCallback,
		AfterEachCallback, BeforeAllCallback, AfterAllCallback {

	/**
	 * The common asserts that can be used on the integration tests.
	 */
	public interface Asserts {

		/**
		 * VErify that the body of the response is of the specified class type.
		 *
		 * @param <T>   type of the context,
		 * @param clazz of the content.
		 * @param res   response to get the body content.
		 *
		 * @return the content of the body.
		 */
		static <T> T assertThatBodyIs(Class<T> clazz, HttpResponse<Buffer> res) {

			try {

				assertThat(res.getHeader(HttpHeaders.CONTENT_TYPE)).isEqualTo(MediaType.APPLICATION_JSON);
				return Json.decodeValue(res.body(), clazz);

			} catch (final Throwable throwable) {

				fail(throwable);
				return null;
			}

		}
	}

	/**
	 * The name of the MongoDB to use by the interaction protocol engine.
	 */
	public static final String WENET_INTERACTION_PROTOCOL_ENGINE_DB_NAME = "wenetInteractionProtocolEngineDB";

	/**
	 * The name of the mongo docker container to use.
	 */
	private static final String MONGO_DOCKER_NAME = "mongo:4.2.3";

	/**
	 * The port for the MongoDB that has to be exported.
	 */
	private static final int EXPORT_MONGODB_PORT = 27017;

	/**
	 * The name of the WeNet profile manager docker container to use.
	 */
	private static final String WENET_PROFILE_MANAGER_DOCKER_NAME = "wenet/profile-manager:0.10.0";

	/**
	 * The name of the WeNet profile manager used on the docker network.
	 */
	private static final String WENET_PROFILE_MANAGER_SERVER_NAME = "profileManager";

	/**
	 * The name of the WeNet profile manager database.
	 */
	private static final String WENET_PROFILE_MANAGER_DB_NAME = "wenetProfileManagerDB";

	/**
	 * The post that export the API.
	 */
	private static final int EXPORT_API_PORT = 8080;

	/**
	 * The name of the WeNet task manager docker container to use.
	 */
	private static final String WENET_TASK_MANAGER_DOCKER_NAME = "wenet/task-manager:0.2.0";

	/**
	 * The name of the WeNet task manager used on the docker network.
	 */
	private static final String WENET_TASK_MANAGER_SERVER_NAME = "taskManager";

	/**
	 * The name of the WeNet task manager database.
	 */
	private static final String WENET_TASK_MANAGER_DB_NAME = "wenetTaskManagerDB";

	/**
	 * The started WeNet interaction protocol engine for do the integration tests.
	 */
	private static WeNetInteractionProtocolEngineContext context;

	/**
	 * Create a new mongo container.
	 *
	 * @param dbName  name of the database to start
	 * @param network that shared between containers.
	 *
	 ** @return the mongo container to the specified database.
	 */
	@SuppressWarnings("resource")
	private static GenericContainer<?> createMongoContainerFor(String dbName, Network network) {

		return new GenericContainer<>(MONGO_DOCKER_NAME).withStartupAttempts(1)
				.withEnv("MONGO_INITDB_ROOT_USERNAME", "root").withEnv("MONGO_INITDB_ROOT_PASSWORD", "password")
				.withEnv("MONGO_INITDB_DATABASE", dbName)
				.withCopyFileToContainer(
						MountableFile.forClasspathResource("eu/internetofus/wenet_task_manager/initialize-" + dbName + ".js"),
						"/docker-entrypoint-initdb.d/init-mongo.js")
				.withExposedPorts(EXPORT_MONGODB_PORT).withNetwork(network).withNetworkAliases(dbName)
				.waitingFor(Wait.forListeningPort());

	}

	/**
	 * Return the defined vertx.
	 *
	 * @return the started vertx.
	 */
	@SuppressWarnings("resource")
	public static synchronized WeNetInteractionProtocolEngineContext getContext() {

		if (context == null) {

			final Semaphore semaphore = new Semaphore(0);
			new Thread(() -> {

				try {

					final Network network = Network.newNetwork();

					final GenericContainer<?> profilePersistenceContainer = createMongoContainerFor(WENET_PROFILE_MANAGER_DB_NAME,
							network);
					profilePersistenceContainer.start();

					final GenericContainer<?> profileManagerContainer = new GenericContainer<>(WENET_PROFILE_MANAGER_DOCKER_NAME)
							.withEnv("DB_HOST", WENET_PROFILE_MANAGER_DB_NAME);
					profileManagerContainer.start();
					final String profileManagerApiPort = String.valueOf(profileManagerContainer.getMappedPort(EXPORT_API_PORT));

					final GenericContainer<?> taskPersistenceContainer = createMongoContainerFor(WENET_TASK_MANAGER_DB_NAME,
							network);
					taskPersistenceContainer.start();
					final GenericContainer<?> taskManagerContainer = new GenericContainer<>(WENET_TASK_MANAGER_DOCKER_NAME)
							.withEnv("DB_HOST", WENET_TASK_MANAGER_DB_NAME)
							.withEnv("WENET_PROFILE_MANAGER_API_HOST", WENET_PROFILE_MANAGER_SERVER_NAME)
							.withEnv("WENET_PROFILE_MANAGER_API_PORT", profileManagerApiPort)
							.withEnv("WENET_PROFILE_MANAGER_API_PATH", "").withNetwork(network)
							.withNetworkAliases(WENET_TASK_MANAGER_SERVER_NAME);

					taskManagerContainer.start();
					final String taskManagerApiPort = String.valueOf(taskManagerContainer.getMappedPort(EXPORT_API_PORT));

					final GenericContainer<?> persistenceContainer = createMongoContainerFor(
							WENET_INTERACTION_PROTOCOL_ENGINE_DB_NAME, network);
					persistenceContainer.start();

					int port = 0;
					try {
						final ServerSocket server = new ServerSocket(0);
						port = server.getLocalPort();
						server.close();
					} catch (final Throwable ignored) {
					}
					new Main()
							.startWith("-papi.port=" + port, "-ppersistence.host=" + WENET_INTERACTION_PROTOCOL_ENGINE_DB_NAME,
									"-ppersistence.port=" + persistenceContainer.getMappedPort(EXPORT_API_PORT),
									"-pservice.profileManager.host=" + WENET_PROFILE_MANAGER_SERVER_NAME,
									"-pservice.profileManager.port=" + profileManagerApiPort, "-pservice.profileManager.apiPath=\"\"",
									"-pservice.taskManager.host=" + WENET_TASK_MANAGER_SERVER_NAME,
									"-pservice.taskManager.port=" + taskManagerApiPort, "-pservice.taskManager.apiPath=\"\"")
							.onComplete(start -> {

								if (start.failed()) {

									InternalLogger.log(Level.ERROR, start.cause(), "Cannot start the WeNet interaction protocol engine");
									profilePersistenceContainer.stop();
									profileManagerContainer.stop();
									taskPersistenceContainer.stop();
									taskManagerContainer.stop();
									persistenceContainer.stop();

								} else {

									context = start.result();
								}

								semaphore.release();

							});

				} catch (final Throwable throwable) {

					InternalLogger.log(Level.ERROR, throwable,
							"Cannot start the required services by WeNet interaction protocol engine");
					semaphore.release();
				}

			}).start();
			try {
				semaphore.acquire();
			} catch (final InterruptedException ignored) {
			}
		}
		return context;

	}

	/**
	 * The extension that manage the vertx components.
	 */
	protected VertxExtension vertxExtension = new VertxExtension();

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void afterAll(ExtensionContext context) throws Exception {

		this.vertxExtension.afterAll(context);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void beforeAll(ExtensionContext context) throws Exception {

		if (getContext() == null) {

			fail("The WeNet interaction protocol engine is not started.");
		}
		this.vertxExtension.beforeAll(context);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void afterEach(ExtensionContext context) throws Exception {

		this.vertxExtension.afterEach(context);

		// close client and pool after the test context has been completed.
		final WebClient client = context.getStore(ExtensionContext.Namespace.create(this.getClass().getName()))
				.remove(WebClient.class.getName(), WebClient.class);
		if (client != null) {

			client.close();
		}

		final MongoClient pool = context.getStore(ExtensionContext.Namespace.create(this.getClass().getName()))
				.remove(MongoClient.class.getName(), MongoClient.class);
		if (pool != null) {

			pool.close();
		}

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void beforeEach(ExtensionContext context) throws Exception {

		this.vertxExtension.beforeEach(context);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void afterTestExecution(ExtensionContext context) throws Exception {

		this.vertxExtension.afterTestExecution(context);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void beforeTestExecution(ExtensionContext context) throws Exception {

		this.vertxExtension.beforeTestExecution(context);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public boolean supportsParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
			throws ParameterResolutionException {

		final Class<?> type = parameterContext.getParameter().getType();
		return type == WebClient.class
				// || type == TasksRepository.class
				|| type == MongoClient.class || type == WeNetProfileManagerService.class
				|| type == WeNetProfileManagerService.class
				|| this.vertxExtension.supportsParameter(parameterContext, extensionContext);

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public Object resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
			throws ParameterResolutionException {

		final Class<?> type = parameterContext.getParameter().getType();
		if (type == Vertx.class) {

			return getContext().vertx;

		} else if (type == WebClient.class) {

			return extensionContext.getStore(ExtensionContext.Namespace.create(this.getClass().getName()))
					.getOrComputeIfAbsent(WebClient.class.getName(), key -> {

						final WeNetInteractionProtocolEngineContext context = getContext();
						final WebClientOptions options = new WebClientOptions();
						options.setDefaultHost(context.configuration.getJsonObject("api").getString("host"));
						options.setDefaultPort(context.configuration.getJsonObject("api").getInteger("port"));
						return WebClient.create(context.vertx, options);
					}, WebClient.class);

		} else if (type == MongoClient.class) {

			final MongoClient pool = extensionContext.getStore(ExtensionContext.Namespace.create(this.getClass().getName()))
					.getOrComputeIfAbsent(MongoClient.class.getName(), key -> {

						final WeNetInteractionProtocolEngineContext context = getContext();
						final JsonObject persitenceConf = context.configuration.getJsonObject("persistence", new JsonObject());
						return MongoClient.create(context.vertx, persitenceConf);
					}, MongoClient.class);
			return pool;

			// } else if (type == TasksRepository.class) {
			//
			// return
			// extensionContext.getStore(ExtensionContext.Namespace.create(this.getClass().getName()))
			// .getOrComputeIfAbsent(TasksRepository.class.getName(), key -> {
			//
			// final WeNetInteractionProtocolEngineContext context = getContext();
			// return TasksRepository.createProxy(context.vertx);
			// }, TasksRepository.class);

		} else if (type == WeNetProfileManagerService.class) {

			return extensionContext.getStore(ExtensionContext.Namespace.create(this.getClass().getName()))
					.getOrComputeIfAbsent(WeNetProfileManagerService.class.getName(), key -> {

						final WeNetInteractionProtocolEngineContext context = getContext();
						return WeNetProfileManagerService.createProxy(context.vertx);

					}, WeNetProfileManagerService.class);

		} else if (type == WeNetInteractionProtocolEngineContext.class) {

			return getContext();

		} else {

			return this.vertxExtension.resolveParameter(parameterContext, extensionContext);
		}
	}

}
