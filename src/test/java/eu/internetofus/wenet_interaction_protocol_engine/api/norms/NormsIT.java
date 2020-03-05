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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import static eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension.Asserts.assertThatBodyIs;
import static eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepositoryTestCase.createAndStoreSomeFakePublishNorms;
import static eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepositoryTestCase.removeAllNorms;
import static io.vertx.junit5.web.TestRequest.queryParam;
import static io.vertx.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.parallel.Execution;
import org.junit.jupiter.api.parallel.ExecutionMode;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.ErrorMessage;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.mongo.MongoClient;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;

/**
 * The integration test over the {@link Norms}.
 *
 * @see Norms
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class NormsIT {

	/**
	 * Verify that return error when search an undefined published norm.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundPublishedNormWithAnUndefinedPublishedNormId(WebClient client,
			VertxTestContext testContext) {

		testRequest(client, HttpMethod.GET, Norms.PATH + "/undefined-published-norm-identifier").expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty();
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).send(testContext);
	}

	/**
	 * Verify that return a defined published norm.
	 *
	 * @param repository  to access the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundPublishedNorm(NormsRepository repository, WebClient client, VertxTestContext testContext) {

		repository.storePublishedNorm(new PublishedNormTest().createModelExample(1), testContext.succeeding(stored -> {

			testRequest(client, HttpMethod.GET, Norms.PATH + "/" + stored._id).expect(res -> testContext.verify(() -> {

				assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
				final PublishedNorm found = assertThatBodyIs(PublishedNorm.class, res);
				assertThat(found).isEqualTo(stored);
				testContext.completeNow();

			})).send(testContext);

		}));

	}

	/**
	 * Verify that can not store a bad published norm.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreANonPublishedNormObject(WebClient client, VertxTestContext testContext) {

		testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty().isEqualTo("bad_publishedNorm");
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).sendJson(new JsonObject().put("udefinedKey", "value"), testContext);
	}

	/**
	 * Verify that can not store a bad published norm.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreBadPublishedNorm(WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm = new PublishedNorm();
		publishedNorm._id = UUID.randomUUID().toString();
		testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty().isEqualTo("bad_publishedNorm._id");
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).sendJson(publishedNorm.toJsonObject(), testContext);
	}

	/**
	 * Verify that store a published norm.
	 *
	 * @param repository     that manage the norms.
	 * @param profileManager service to create user profiles.
	 * @param client         to connect to the server.
	 * @param testContext    context to test.
	 *
	 * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStorePublishedNorm(NormsRepository repository, WeNetProfileManagerService profileManager,
			WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm = new PublishedNormTest().createModelExample(1);
		testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
			final PublishedNorm stored = assertThatBodyIs(PublishedNorm.class, res);
			assertThat(stored).isNotNull().isNotEqualTo(publishedNorm);
			publishedNorm._id = stored._id;
			publishedNorm.publishTime = stored.publishTime;
			assertThat(stored).isEqualTo(publishedNorm);
			repository.searchPublishedNorm(stored._id, testContext.succeeding(foundPublishedNorm -> testContext.verify(() -> {

				assertThat(foundPublishedNorm).isEqualTo(stored);
				testContext.completeNow();

			})));

		}).sendJson(publishedNorm.toJsonObject(), testContext);

	}

	/**
	 * Verify that store an empty published norm.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreEmptyPublishedNorm(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		final PublishedNorm publishedNorm = new PublishedNorm();
		testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
			final PublishedNorm stored = assertThatBodyIs(PublishedNorm.class, res);
			assertThat(stored).isNotNull().isNotEqualTo(publishedNorm);
			publishedNorm._id = stored._id;
			publishedNorm.publishTime = stored.publishTime;
			assertThat(stored).isEqualTo(publishedNorm);
			repository.searchPublishedNorm(stored._id, testContext.succeeding(foundPublishedNorm -> testContext.verify(() -> {

				assertThat(foundPublishedNorm).isEqualTo(stored);
				testContext.completeNow();

			})));

		}).sendJson(publishedNorm.toJsonObject(), testContext);

	}

	/**
	 * Verify that store a simple published norm.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreSimplePublishedNorm(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		final PublishedNorm publishedNorm = new PublishedNormTest().createModelExample(1);
		testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
			final PublishedNorm stored = assertThatBodyIs(PublishedNorm.class, res);
			assertThat(stored).isNotNull().isNotEqualTo(publishedNorm);
			publishedNorm._id = stored._id;
			assertThat(stored).isNotNull().isNotEqualTo(publishedNorm);
			publishedNorm.publishTime = stored.publishTime;
			assertThat(stored).isEqualTo(publishedNorm);
			repository.searchPublishedNorm(stored._id, testContext.succeeding(foundPublishedNorm -> testContext.verify(() -> {

				assertThat(foundPublishedNorm).isEqualTo(stored);
				testContext.completeNow();

			})));

		}).sendJson(publishedNorm.toJsonObject(), testContext);

	}

	/**
	 * Verify that return error when try to update an undefined publishedNorm.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormThatIsNotDefined(WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm = new PublishedNormTest().createModelExample(1);
		testRequest(client, HttpMethod.PUT, Norms.PATH + "/undefined-publishedNorm-identifier").expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty();
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).sendJson(publishedNorm.toJsonObject(), testContext);
	}

	/**
	 * Verify that return error when try to update with a model that is not a
	 * publishedNorm.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormWithANotPublishedNormObject(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
				testContext.succeeding(publishedNorm -> {

					testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + publishedNorm._id).expect(res -> {

						assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
						final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
						assertThat(error.code).isNotEmpty();
						assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
						testContext.completeNow();

					}).sendJson(new JsonObject().put("udefinedKey", "value"), testContext);
				}));
	}

	/**
	 * Verify that not update a published norm if any change is done.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormBecauseNotChangesHasDone(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
				testContext.succeeding(publishedNorm -> {

					testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + publishedNorm._id).expect(res -> {

						assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
						final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
						assertThat(error.code).isNotEmpty();
						assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
						testContext.completeNow();

					}).sendJson(new JsonObject(), testContext);
				}));

	}

	/**
	 * Verify that not update a published norm because the source is not valid.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormBecauseBadSource(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
				testContext.succeeding(publishedNorm -> {

					testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + publishedNorm._id).expect(res -> {

						assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
						final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
						assertThat(error.code).isNotEmpty().endsWith(".publisherId");
						assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
						testContext.completeNow();

					}).sendJson(new JsonObject().put("publisherId", "image.png"), testContext);
				}));

	}

	/**
	 * Verify that can update a complex published norm with another.
	 *
	 * @param profileManager service to manage the profiles.
	 * @param repository     that manage the norms.
	 * @param client         to connect to the server.
	 * @param testContext    context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdatePublishedNorm(WeNetProfileManagerService profileManager, NormsRepository repository,
			WebClient client, VertxTestContext testContext) {

		PublishedNormTest.createValidPublishedNormExample(23, profileManager, testContext.succeeding(created -> {

			repository.storePublishedNorm(created, testContext.succeeding(storedPublishedNorm -> {

				final PublishedNorm newPublishedNorm = new PublishedNormTest().createModelExample(2);
				newPublishedNorm._id = UUID.randomUUID().toString();
				testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + storedPublishedNorm._id)
						.expect(res -> testContext.verify(() -> {

							assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
							final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
							assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
							newPublishedNorm._id = storedPublishedNorm._id;
							newPublishedNorm.publishTime = storedPublishedNorm.publishTime;
							assertThat(updated).isEqualTo(newPublishedNorm);
							testContext.completeNow();

						})).sendJson(newPublishedNorm.toJsonObject(), testContext);
			}));
		}));
	}

	/**
	 * Verify that return error when delete an undefined published norm.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotDeletePublishedNormWithAnUndefinedPublishedNormId(WebClient client,
			VertxTestContext testContext) {

		testRequest(client, HttpMethod.DELETE, Norms.PATH + "/undefined-publishedNorm-identifier").expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty();
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).send(testContext);
	}

	/**
	 * Verify that can delete a published norm.
	 *
	 * @param repository  to access the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeletePublishedNorm(NormsRepository repository, WebClient client, VertxTestContext testContext) {

		repository.storePublishedNorm(new PublishedNorm(), testContext.succeeding(storedPublishedNorm -> {

			testRequest(client, HttpMethod.DELETE, Norms.PATH + "/" + storedPublishedNorm._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.NO_CONTENT.getStatusCode());
						testContext.completeNow();

					})).send(testContext);

		}));

	}

	/**
	 * Verify that can update only the published norm name.
	 *
	 * @param profileManager service to manage the profiles.
	 * @param repository     that manage the norms.
	 * @param client         to connect to the server.
	 * @param testContext    context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdatePublishedNormName(WeNetProfileManagerService profileManager, NormsRepository repository,
			WebClient client, VertxTestContext testContext) {

		PublishedNormTest.createValidPublishedNormExample(23, profileManager, testContext.succeeding(created -> {
			repository.storePublishedNorm(created, testContext.succeeding(storedPublishedNorm -> {

				final PublishedNorm newPublishedNorm = new PublishedNorm();
				newPublishedNorm._id = UUID.randomUUID().toString();
				newPublishedNorm.name = "New publishedNorm name";
				testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + storedPublishedNorm._id)
						.expect(res -> testContext.verify(() -> {

							assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
							final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
							assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
							storedPublishedNorm.name = "New publishedNorm name";
							assertThat(updated).isEqualTo(storedPublishedNorm);
							testContext.completeNow();

						})).sendJson(newPublishedNorm.toJsonObject(), testContext);
			}));
		}));
	}

	/**
	 * Verify that can update only the published norm description.
	 *
	 * @param profileManager service to manage the profiles.
	 * @param repository     that manage the norms.
	 * @param client         to connect to the server.
	 * @param testContext    context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdatePublishedNormDescription(WeNetProfileManagerService profileManager,
			NormsRepository repository, WebClient client, VertxTestContext testContext) {

		PublishedNormTest.createValidPublishedNormExample(23, profileManager, testContext.succeeding(created -> {
			repository.storePublishedNorm(created, testContext.succeeding(storedPublishedNorm -> {

				final PublishedNorm newPublishedNorm = new PublishedNorm();
				newPublishedNorm._id = UUID.randomUUID().toString();
				newPublishedNorm.description = "New publishedNorm description";
				testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + storedPublishedNorm._id)
						.expect(res -> testContext.verify(() -> {

							assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
							final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
							assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
							storedPublishedNorm.description = "New publishedNorm description";
							assertThat(updated).isEqualTo(storedPublishedNorm);
							testContext.completeNow();

						})).sendJson(newPublishedNorm.toJsonObject(), testContext);
			}));
		}));
	}

	/**
	 * Verify that can update the keywords of a published norm.
	 *
	 * @param profileManager service to manage the profiles.
	 * @param repository     to access the norms.
	 * @param client         to connect to the server.
	 * @param testContext    context to test.
	 *
	 * @see Norms#updatePublishedNorm(String, JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdatePublishedNormKeyword(WeNetProfileManagerService profileManager, NormsRepository repository,
			WebClient client, VertxTestContext testContext) {

		PublishedNormTest.createValidPublishedNormExample(23, profileManager, testContext.succeeding(created -> {
			repository.storePublishedNorm(created, testContext.succeeding(storedPublishedNorm -> {

				final PublishedNorm newPublishedNorm = new PublishedNorm();
				newPublishedNorm.keywords = new ArrayList<>();
				newPublishedNorm.keywords.add("    ");
				newPublishedNorm.keywords.add("New keyword");
				testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + storedPublishedNorm._id)
						.expect(res -> testContext.verify(() -> {

							assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
							final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
							assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);

							storedPublishedNorm.keywords.clear();
							storedPublishedNorm.keywords.add("New keyword");
							assertThat(updated).isEqualTo(storedPublishedNorm);

						})).sendJson(newPublishedNorm.toJsonObject(), testContext);
			}));
		}));

	}

	/**
	 * Verify that can update only the published norm publisherId.
	 *
	 * @param profileManager service to manage the profiles.
	 * @param repository     that manage the norms.
	 * @param client         to connect to the server.
	 * @param testContext    context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdatePublishedNormPublisherId(WeNetProfileManagerService profileManager,
			NormsRepository repository, WebClient client, VertxTestContext testContext) {

		PublishedNormTest.createValidPublishedNormExample(23, profileManager, testContext.succeeding(created -> {
			repository.storePublishedNorm(created, testContext.succeeding(storedPublishedNorm -> {

				final PublishedNorm newPublishedNorm = new PublishedNorm();
				newPublishedNorm._id = UUID.randomUUID().toString();
				newPublishedNorm.publisherId = "http://host.com/newPublisherId.png";
				testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + storedPublishedNorm._id)
						.expect(res -> testContext.verify(() -> {

							assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
							final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
							assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
							storedPublishedNorm.publisherId = "http://host.com/newPublisherId.png";
							assertThat(updated).isEqualTo(storedPublishedNorm);
							testContext.completeNow();

						})).sendJson(newPublishedNorm.toJsonObject(), testContext);
			}));
		}));
	}

	/**
	 * Verify that not update a published norm publish time.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormPublishTime(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
				testContext.succeeding(publishedNorm -> {

					testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + publishedNorm._id).expect(res -> {

						assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
						final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
						assertThat(error.code).isNotEmpty().doesNotContain(".publishTime");
						assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
						testContext.completeNow();

					}).sendJson(new JsonObject().put("publishTime", 0l), testContext);
				}));

	}

	/**
	 * Verify that not update a published norm publish time.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdatePublishedNormId(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
				testContext.succeeding(publishedNorm -> {

					testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + publishedNorm._id).expect(res -> {

						assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
						final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
						assertThat(error.code).isNotEmpty().doesNotContain("._id");
						assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
						testContext.completeNow();

					}).sendJson(new JsonObject().put("_id", "Identifier"), testContext);
				}));

	}

	/**
	 * Verify that found some norms by its name.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByName(NormsRepository repository, WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String name = UUID.randomUUID().toString();
		publishedNorm1.name = name + " 1";
		repository.storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

			final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
			publishedNorm2.name = name + " 1";
			repository.storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

				testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", name)).expect(res -> {

					assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
					final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
					assertThat(page.offset).isEqualTo(0);
					assertThat(page.total).isEqualTo(2);
					assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1, storedPublishedNorm2);
					testContext.completeNow();

				}).send(testContext);
			}));
		}));
	}

	/**
	 * Verify that found some norms by its description.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByDescription(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String description = UUID.randomUUID().toString();
		publishedNorm1.description += description;
		repository.storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

			final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
			publishedNorm2.description = description + " " + publishedNorm2.description;
			repository.storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

				testRequest(client, HttpMethod.GET, Norms.PATH)
						.with(queryParam("description", description), queryParam("offset", "1")).expect(res -> {

							assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
							final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
							assertThat(page.offset).isEqualTo(1);
							assertThat(page.total).isEqualTo(2);
							assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm2);
							testContext.completeNow();

						}).send(testContext);
			}));
		}));
	}

	/**
	 * Verify that found some norms by a keyword.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByAKeyword(NormsRepository repository, WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String keyword = UUID.randomUUID().toString();
		publishedNorm1.keywords.add(keyword);
		repository.storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

			final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
			publishedNorm2.keywords.add(keyword);
			repository.storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

				testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("keyword", keyword), queryParam("limit", "1"))
						.expect(res -> {

							assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
							final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
							assertThat(page.offset).isEqualTo(0);
							assertThat(page.total).isEqualTo(2);
							assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1);
							testContext.completeNow();

						}).send(testContext);
			}));
		}));
	}

	/**
	 * Verify that found some norms by some keyword.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsBySomeKeyword(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String keyword = UUID.randomUUID().toString();
		publishedNorm1.keywords.add(keyword);
		repository.storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

			final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
			publishedNorm2.keywords.add(1, keyword);
			repository.storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

				final PublishedNorm publishedNorm3 = new PublishedNormTest().createModelExample(30);
				publishedNorm3.keywords.add(0, keyword);
				repository.storePublishedNorm(publishedNorm3, testContext.succeeding(storedPublishedNorm3 -> {

					testRequest(client, HttpMethod.GET, Norms.PATH)
							.with(queryParam("keyword", keyword), queryParam("keyword", "keyword 1")).expect(res -> {

								assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
								final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
								assertThat(page.offset).isEqualTo(0);
								assertThat(page.total).isEqualTo(2);
								assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1, storedPublishedNorm2);
								testContext.completeNow();

							}).send(testContext);
				}));
			}));
		}));
	}

	/**
	 * Verify that found some norms by its publisherId.
	 *
	 * @param repository  that manage the norms.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByPublisherId(NormsRepository repository, WebClient client,
			VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String publisherId = "http://host.com/publisherId_" + UUID.randomUUID().toString() + ".png";
		publishedNorm1.publisherId = publisherId;
		repository.storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

			final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
			publishedNorm2.publisherId = publisherId;
			repository.storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

				testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("publisherId", publisherId)).expect(res -> {

					assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
					final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
					assertThat(page.offset).isEqualTo(0);
					assertThat(page.total).isEqualTo(2);
					assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1, storedPublishedNorm2);
					testContext.completeNow();

				}).send(testContext);
			}));
		}));
	}

	/**
	 * Verify that found some norms by its publish from.
	 *
	 * @param pool        that create the mongo connections.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFoundNormsByPublishFrom(MongoClient pool, WebClient client, VertxTestContext testContext) {

		removeAllNorms(pool);
		final List<PublishedNorm> norms = createAndStoreSomeFakePublishNorms(pool, 23);

		testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("publishFrom", "0"), queryParam("offset", "7"))
				.expect(res -> {

					assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
					final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
					assertThat(page.offset).isEqualTo(7);
					assertThat(page.total).isEqualTo(23);
					assertThat(page.norms).isEqualTo(norms.subList(7, 17));
					testContext.completeNow();

				}).send(testContext);
	}

	/**
	 * Verify that found some norms by its publish to.
	 *
	 * @param pool        that create the mongo connections.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFoundNormsByPublishTo(MongoClient pool, WebClient client, VertxTestContext testContext) {

		removeAllNorms(pool);
		final List<PublishedNorm> norms = createAndStoreSomeFakePublishNorms(pool, 23);

		testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("publishTo", "10000000"), queryParam("limit", "7"))
				.expect(res -> {

					assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
					final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
					assertThat(page.offset).isEqualTo(0);
					assertThat(page.total).isEqualTo(23);
					assertThat(page.norms).isEqualTo(norms.subList(0, 7));
					testContext.completeNow();

				}).send(testContext);
	}

	/**
	 * Verify that found some norms by its publish range.
	 *
	 * @param pool        that create the mongo connections.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	@Execution(ExecutionMode.SAME_THREAD)
	public void shouldFoundNormsByPublishRange(MongoClient pool, WebClient client, VertxTestContext testContext) {

		removeAllNorms(pool);
		final List<PublishedNorm> norms = createAndStoreSomeFakePublishNorms(pool, 23);

		testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("publishFrom", "700000"),
				queryParam("publishTo", "1300000"), queryParam("offset", "1"), queryParam("limit", "3")).expect(res -> {

					assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
					final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
					assertThat(page.offset).isEqualTo(1);
					assertThat(page.total).isEqualTo(7);
					assertThat(page.norms).isEqualTo(norms.subList(8, 11));
					testContext.completeNow();

				}).send(testContext);
	}

	/**
	 * Verify that found a published norm.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundNormsBecausePatternIsNotValid(WebClient client, VertxTestContext testContext) {

		testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", "a{12(")).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty();
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).send(testContext);
	}

	/**
	 * Verify that return an empty published norms if any match.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldEmptyPageIfAnyPublishedNormMatch(WebClient client, VertxTestContext testContext) {

		testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", UUID.randomUUID().toString()))
				.expect(res -> {

					assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
					final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
					assertThat(page.offset).isEqualTo(0);
					assertThat(page.total).isEqualTo(0);
					assertThat(page.norms).isNull();
					testContext.completeNow();

				}).send(testContext);
	}

}
