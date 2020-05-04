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

import static eu.internetofus.common.api.HttpResponses.assertThatBodyIs;
import static io.vertx.junit5.web.TestRequest.queryParam;
import static io.vertx.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.UUID;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.api.models.ErrorMessage;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
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
	 * @param vertx       event bus to use.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNorm(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundPublishedNorm(Vertx vertx, WebClient client, VertxTestContext testContext) {

		NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNormTest().createModelExample(1),
				testContext.succeeding(stored -> {

					testRequest(client, HttpMethod.GET, Norms.PATH + "/" + stored.id).expect(res -> testContext.verify(() -> {

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

	// /**
	// * Verify that can not store a bad published norm.
	// *
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldNotStoreBadPublishedNorm(WebClient client, VertxTestContext
	// testContext) {
	//
	// final PublishedNorm publishedNorm = createMinimumValidPublishedNormExample();
	// publishedNorm._id = UUID.randomUUID().toString();
	// testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty().isEqualTo("bad_publishedNorm._id");
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).sendJson(publishedNorm.toJsonObject(), testContext);
	// }
	//
	// /**
	// * Verify that store a published norm.
	// *
	// * @param repository that manage the norms.
	// * @param profileManager service to create user profiles.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldStorePublishedNorm(Vertx vertx,
	// WeNetProfileManagerService profileManager,
	// WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(publishedNorm -> {
	//
	// testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm stored = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(stored).isNotNull().isNotEqualTo(publishedNorm);
	// publishedNorm._id = stored._id;
	// publishedNorm.publishTime = stored.publishTime;
	// publishedNorm.norm.id = stored.norm.id;
	// assertThat(stored).isEqualTo(publishedNorm);
	// repository.searchPublishedNorm(stored._id,
	// testContext.succeeding(foundPublishedNorm -> testContext.verify(() -> {
	//
	// assertThat(foundPublishedNorm).isEqualTo(stored);
	// testContext.completeNow();
	//
	// })));
	//
	// }).sendJson(publishedNorm.toJsonObject(), testContext);
	//
	// }));
	//
	// }
	//
	// /**
	// * Verify that store an empty published norm.
	// *
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#publishNorm(io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldStoreMinimumValidPublishedNorm(Vertx vertx,
	// WebClient client,
	// VertxTestContext testContext) {
	//
	// final PublishedNorm publishedNorm = createMinimumValidPublishedNormExample();
	// testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm stored = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(stored).isNotNull().isNotEqualTo(publishedNorm);
	// publishedNorm._id = stored._id;
	// publishedNorm.publishTime = stored.publishTime;
	// publishedNorm.norm.id = stored.norm.id;
	// assertThat(stored).isEqualTo(publishedNorm);
	// repository.searchPublishedNorm(stored._id,
	// testContext.succeeding(foundPublishedNorm -> testContext.verify(() -> {
	//
	// assertThat(foundPublishedNorm).isEqualTo(stored);
	// testContext.completeNow();
	//
	// })));
	//
	// }).sendJson(publishedNorm.toJsonObject(), testContext);
	//
	// }
	//
	// /**
	// * Verify that return error when try to update an undefined publishedNorm.
	// *
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldNotUpdatePublishedNormThatIsNotDefined(WebClient client,
	// VertxTestContext testContext) {
	//
	// final PublishedNorm publishedNorm = new
	// PublishedNormTest().createModelExample(1);
	// testRequest(client, HttpMethod.PUT, Norms.PATH +
	// "/undefined-publishedNorm-identifier").expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty();
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).sendJson(publishedNorm.toJsonObject(), testContext);
	// }
	//
	// /**
	// * Verify that return error when try to update with a model that is not a
	// * publishedNorm.
	// *
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void
	// shouldNotUpdatePublishedNormWithANotPublishedNormObject(NormsRepository
	// repository, WebClient client,
	// VertxTestContext testContext) {
	//
	// repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
	// testContext.succeeding(publishedNorm -> {
	//
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// publishedNorm._id).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty();
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).sendJson(new JsonObject().put("udefinedKey", "value"), testContext);
	// }));
	// }
	//
	// /**
	// * Verify that not update a published norm if any change is done.
	// *
	// * @param profileManager service to manage the user profiles.
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void
	// shouldNotUpdatePublishedNormBecauseNotChangesHasDone(WeNetProfileManagerService
	// profileManager,
	// Vertx vertx, WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(1, profileManager,
	// testContext.succeeding(created -> {
	//
	// repository.storePublishedNorm(created, testContext.succeeding(publishedNorm
	// -> {
	//
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// publishedNorm._id).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isEqualTo("published_norm_to_update_equal_to_original");
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).sendJson(new JsonObject().put("norm", new Norm().toJsonObject()),
	// testContext);
	// }));
	//
	// }));
	//
	// }
	//
	// /**
	// * Verify that not update a published norm because the source is not valid.
	// *
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldNotUpdatePublishedNormBecauseBadSource(NormsRepository
	// repository, WebClient client,
	// VertxTestContext testContext) {
	//
	// repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
	// testContext.succeeding(publishedNorm -> {
	//
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// publishedNorm._id).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty().endsWith(".publisherId");
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).sendJson(new JsonObject().put("publisherId", "image.png"), testContext);
	// }));
	//
	// }
	//
	// /**
	// * Verify that can update a complex published norm with another.
	// *
	// * @param profileManager service to manage the profiles.
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldUpdatePublishedNorm(WeNetProfileManagerService
	// profileManager, Vertx vertx,
	// WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(created -> {
	//
	// repository.storePublishedNorm(created,
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// final PublishedNorm newPublishedNorm = new PublishedNorm();
	// newPublishedNorm.norm = new NormTest().createModelExample(45);
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
	// storedPublishedNorm.norm = new NormTest().createModelExample(45);
	// storedPublishedNorm.norm.id = updated.norm.id;
	// assertThat(updated).isEqualTo(storedPublishedNorm);
	// testContext.completeNow();
	//
	// })).sendJson(newPublishedNorm.toJsonObject(), testContext);
	// }));
	// }));
	// }
	//
	// /**
	// * Verify that return error when delete an undefined published norm.
	// *
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void
	// shouldNotDeletePublishedNormWithAnUndefinedPublishedNormId(WebClient client,
	// VertxTestContext testContext) {
	//
	// testRequest(client, HttpMethod.DELETE, Norms.PATH +
	// "/undefined-publishedNorm-identifier").expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty();
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).send(testContext);
	// }
	//
	// /**
	// * Verify that can delete a published norm.
	// *
	// * @param repository to access the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldDeletePublishedNorm(Vertx vertx, WebClient
	// client, VertxTestContext testContext) {
	//
	// repository.storePublishedNorm(new PublishedNorm(),
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// testRequest(client, HttpMethod.DELETE, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.NO_CONTENT.getStatusCode());
	// testContext.completeNow();
	//
	// })).send(testContext);
	//
	// }));
	//
	// }
	//
	// /**
	// * Verify that can update only the published norm name.
	// *
	// * @param profileManager service to manage the profiles.
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldUpdatePublishedNormName(WeNetProfileManagerService
	// profileManager, Vertx vertx,
	// WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(created -> {
	// repository.storePublishedNorm(created,
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// final PublishedNorm newPublishedNorm = new PublishedNorm();
	// newPublishedNorm._id = UUID.randomUUID().toString();
	// newPublishedNorm.name = "New publishedNorm name";
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
	// storedPublishedNorm.name = "New publishedNorm name";
	// assertThat(updated).isEqualTo(storedPublishedNorm);
	// testContext.completeNow();
	//
	// })).sendJson(newPublishedNorm.toJsonObject(), testContext);
	// }));
	// }));
	// }
	//
	// /**
	// * Verify that can update only the published norm description.
	// *
	// * @param profileManager service to manage the profiles.
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldUpdatePublishedNormDescription(WeNetProfileManagerService
	// profileManager,
	// Vertx vertx, WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(created -> {
	// repository.storePublishedNorm(created,
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// final PublishedNorm newPublishedNorm = new PublishedNorm();
	// newPublishedNorm._id = UUID.randomUUID().toString();
	// newPublishedNorm.description = "New publishedNorm description";
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
	// storedPublishedNorm.description = "New publishedNorm description";
	// assertThat(updated).isEqualTo(storedPublishedNorm);
	// testContext.completeNow();
	//
	// })).sendJson(newPublishedNorm.toJsonObject(), testContext);
	// }));
	// }));
	// }
	//
	// /**
	// * Verify that can update the keywords of a published norm.
	// *
	// * @param profileManager service to manage the profiles.
	// * @param repository to access the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#updatePublishedNorm(String, JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldUpdatePublishedNormKeyword(WeNetProfileManagerService
	// profileManager, Vertx vertx,
	// WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(created -> {
	// repository.storePublishedNorm(created,
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// final PublishedNorm newPublishedNorm = new PublishedNorm();
	// newPublishedNorm.keywords = new ArrayList<>();
	// newPublishedNorm.keywords.add(" ");
	// newPublishedNorm.keywords.add(" New keyword ");
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
	//
	// storedPublishedNorm.keywords.clear();
	// storedPublishedNorm.keywords.add("New keyword");
	// assertThat(updated).isEqualTo(storedPublishedNorm);
	//
	// })).sendJson(newPublishedNorm.toJsonObject(), testContext);
	// }));
	// }));
	//
	// }
	//
	// /**
	// * Verify that can update only the published norm publisherId.
	// *
	// * @param profileManager service to manage the profiles.
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldUpdatePublishedNormPublisherId(WeNetProfileManagerService
	// profileManager,
	// Vertx vertx, WebClient client, VertxTestContext testContext) {
	//
	// profileManager.createProfile(new JsonObject(),
	// testContext.succeeding(createdProfile -> {
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(created -> {
	// repository.storePublishedNorm(created,
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// final PublishedNorm newPublishedNorm = new PublishedNorm();
	// newPublishedNorm._id = UUID.randomUUID().toString();
	// newPublishedNorm.publisherId = createdProfile.getString("id");
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
	// storedPublishedNorm.publisherId = createdProfile.getString("id");
	// assertThat(updated).isEqualTo(storedPublishedNorm);
	// testContext.completeNow();
	//
	// })).sendJson(newPublishedNorm.toJsonObject(), testContext);
	// }));
	// }));
	// }));
	// }
	//
	// /**
	// * Verify that not update a published norm publish time.
	// *
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldNotUpdatePublishedNormPublishTime(NormsRepository
	// repository, WebClient client,
	// VertxTestContext testContext) {
	//
	// repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
	// testContext.succeeding(publishedNorm -> {
	//
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// publishedNorm._id).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty().doesNotContain(".publishTime");
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).sendJson(new JsonObject().put("publishTime", 0l), testContext);
	// }));
	//
	// }
	//
	// /**
	// * Verify that not update a published norm publish time.
	// *
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#updatePublishedNorm(String, io.vertx.core.json.JsonObject,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldNotUpdatePublishedNormId(Vertx vertx,
	// WebClient client,
	// VertxTestContext testContext) {
	//
	// repository.storePublishedNorm(new PublishedNormTest().createModelExample(1),
	// testContext.succeeding(publishedNorm -> {
	//
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// publishedNorm._id).expect(res -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty().doesNotContain("._id");
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// }).sendJson(new JsonObject().put("_id", "Identifier"), testContext);
	// }));
	//
	// }
	//
	// /**
	// * Verify that can update the norm in a published norm.
	// *
	// * @param profileManager service to manage the profiles.
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldUpdateNormOnPublishedNorm(WeNetProfileManagerService
	// profileManager, Vertx vertx,
	// WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(created -> {
	// repository.storePublishedNorm(created,
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// final PublishedNorm newPublishedNorm = new PublishedNorm();
	// newPublishedNorm.norm = new NormTest().createModelExample(43);
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
	// final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
	// assertThat(updated).isNotEqualTo(storedPublishedNorm).isNotEqualTo(newPublishedNorm);
	// storedPublishedNorm.norm = new NormTest().createModelExample(43);
	// storedPublishedNorm.norm.id = storedPublishedNorm.norm.id;
	// assertThat(updated).isEqualTo(storedPublishedNorm);
	// testContext.completeNow();
	//
	// })).sendJson(newPublishedNorm.toJsonObject(), testContext);
	// }));
	// }));
	// }
	//
	// /**
	// * Verify that can not update the norm in a published norm.
	// *
	// * @param profileManager service to manage the profiles.
	// * @param repository that manage the norms.
	// * @param client to connect to the server.
	// * @param testContext context to test.
	// *
	// * @see Norms#retrievePublishedNorm(String,
	// * io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	// */
	// @Test
	// public void shouldNotUpdateNormOnPublishedNorm(WeNetProfileManagerService
	// profileManager, Vertx vertx,
	// WebClient client, VertxTestContext testContext) {
	//
	// PublishedNormTest.createValidPublishedNormExample(23, profileManager,
	// testContext.succeeding(created -> {
	// repository.storePublishedNorm(created,
	// testContext.succeeding(storedPublishedNorm -> {
	//
	// final PublishedNorm newPublishedNorm = new PublishedNorm();
	// newPublishedNorm.norm = new Norm();
	// newPublishedNorm.norm.attribute = ValidationsTest.STRING_256;
	// testRequest(client, HttpMethod.PUT, Norms.PATH + "/" +
	// storedPublishedNorm._id)
	// .expect(res -> testContext.verify(() -> {
	//
	// assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
	// final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
	// assertThat(error.code).isNotEmpty().endsWith(".norm.attribute");
	// assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
	// testContext.completeNow();
	//
	// })).sendJson(newPublishedNorm.toJsonObject(), testContext);
	// }));
	// }));
	// }

	/**
	 * Verify that found some norms by its name.
	 *
	 * @param vertx       event bus to use.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByName(Vertx vertx, WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String name = UUID.randomUUID().toString();
		publishedNorm1.name = name + " 1";
		NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1,
				testContext.succeeding(storedPublishedNorm1 -> {

					final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
					publishedNorm2.name += " " + name + " 2";
					NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2,
							testContext.succeeding(storedPublishedNorm2 -> {

								testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", ".*" + name + ".*"))
										.expect(res -> {

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
	 * @param vertx       event bus to use.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByDescription(Vertx vertx, WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String description = UUID.randomUUID().toString();
		publishedNorm1.description += description;
		NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1,
				testContext.succeeding(storedPublishedNorm1 -> {

					final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
					publishedNorm2.description = description + " " + publishedNorm2.description;
					NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2,
							testContext.succeeding(storedPublishedNorm2 -> {

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
	 * @param vertx       event bus to use.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByAKeyword(Vertx vertx, WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String keyword = UUID.randomUUID().toString();
		publishedNorm1.keywords.add(keyword);
		NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1,
				testContext.succeeding(storedPublishedNorm1 -> {

					final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
					publishedNorm2.keywords.add(keyword);
					NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2,
							testContext.succeeding(storedPublishedNorm2 -> {

								testRequest(client, HttpMethod.GET, Norms.PATH)
										.with(queryParam("keyword", keyword), queryParam("limit", "1")).expect(res -> {

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
	 * @param vertx       event bus to use.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsBySomeKeyword(Vertx vertx, WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String keyword = UUID.randomUUID().toString();
		publishedNorm1.keywords.add(keyword);
		final NormsRepository repository = NormsRepository.createProxy(vertx);
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
	 * @param vertx       event bus to use.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Norms#retrievePublishedNormsPage(io.vertx.ext.web.api.OperationRequest,
	 *      io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundNormsByPublisherId(Vertx vertx, WebClient client, VertxTestContext testContext) {

		final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
		final String publisherId = "http://host.com/publisherId_" + UUID.randomUUID().toString() + ".png";
		publishedNorm1.publisherId = publisherId;
		final NormsRepository repository = NormsRepository.createProxy(vertx);
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
