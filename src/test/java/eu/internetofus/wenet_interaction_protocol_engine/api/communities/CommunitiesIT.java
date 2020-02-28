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

package eu.internetofus.wenet_interaction_protocol_engine.api.communities;

import static eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension.Asserts.assertThatBodyIs;
import static io.vertx.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.UUID;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.api.ErrorMessage;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.CommunitiesRepository;
import eu.internetofus.wenet_interaction_protocol_engine.services.WeNetProfileManagerService;
import io.vertx.core.http.HttpMethod;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;

/**
 * The integration test over the {@link Communities}.
 *
 * @see Communities
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class CommunitiesIT {

	/**
	 * Verify that return error when search an undefined community.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotFoundCommunityWithAnUndefinedCommunityId(WebClient client, VertxTestContext testContext) {

		testRequest(client, HttpMethod.GET, Communities.PATH + "/undefined-community-identifier").expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty();
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).send(testContext);
	}

	/**
	 * Verify that return a defined community.
	 *
	 * @param repository  to access the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldFoundCommunity(CommunitiesRepository repository, WebClient client, VertxTestContext testContext) {

		repository.storeCommunity(new CommunityTest().createModelExample(1), testContext.succeeding(community -> {

			testRequest(client, HttpMethod.GET, Communities.PATH + "/" + community._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
						final Community found = assertThatBodyIs(Community.class, res);
						assertThat(found).isEqualTo(community);
						testContext.completeNow();

					})).send(testContext);

		}));

	}

	/**
	 * Verify that can not store a bad community.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#createCommunity(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreANonCommunityObject(WebClient client, VertxTestContext testContext) {

		testRequest(client, HttpMethod.POST, Communities.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty().isEqualTo("bad_community");
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).sendJson(new JsonObject().put("udefinedKey", "value"), testContext);
	}

	/**
	 * Verify that can not store a bad community.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#createCommunity(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotStoreBadCommunity(WebClient client, VertxTestContext testContext) {

		final Community community = new Community();
		community._id = UUID.randomUUID().toString();
		testRequest(client, HttpMethod.POST, Communities.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty().isEqualTo("bad_community._id");
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).sendJson(community.toJsonObject(), testContext);
	}

	/**
	 * Verify that store a community.
	 *
	 * @param repository     that manage the communities.
	 * @param profileManager service to create user profiles.
	 * @param client         to connect to the server.
	 * @param testContext    context to test.
	 *
	 * @see Communities#createCommunity(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreCommunity(CommunitiesRepository repository, WeNetProfileManagerService profileManager,
			WebClient client, VertxTestContext testContext) {

		final Community community = new CommunityTest().createModelExample(1);
		testRequest(client, HttpMethod.POST, Communities.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
			final Community stored = assertThatBodyIs(Community.class, res);
			assertThat(stored).isNotNull().isNotEqualTo(community);
			community._id = stored._id;
			community.sinceTime = stored.sinceTime;
			assertThat(stored).isEqualTo(community);
			repository.searchCommunity(stored._id, testContext.succeeding(foundCommunity -> testContext.verify(() -> {

				assertThat(foundCommunity).isEqualTo(stored);
				testContext.completeNow();

			})));

		}).sendJson(community.toJsonObject(), testContext);

	}

	/**
	 * Verify that store an empty community.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#createCommunity(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreEmptyCommunity(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		final Community community = new Community();
		testRequest(client, HttpMethod.POST, Communities.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
			final Community stored = assertThatBodyIs(Community.class, res);
			assertThat(stored).isNotNull().isNotEqualTo(community);
			community._id = stored._id;
			community.sinceTime = stored.sinceTime;
			assertThat(stored).isEqualTo(community);
			repository.searchCommunity(stored._id, testContext.succeeding(foundCommunity -> testContext.verify(() -> {

				assertThat(foundCommunity).isEqualTo(stored);
				testContext.completeNow();

			})));

		}).sendJson(community.toJsonObject(), testContext);

	}

	/**
	 * Verify that store a simple community.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#createCommunity(io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldStoreSimpleCommunity(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		final Community community = new CommunityTest().createModelExample(1);
		testRequest(client, HttpMethod.POST, Communities.PATH).expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
			final Community stored = assertThatBodyIs(Community.class, res);
			assertThat(stored).isNotNull().isNotEqualTo(community);
			community._id = stored._id;
			assertThat(stored).isNotNull().isNotEqualTo(community);
			community.sinceTime = stored.sinceTime;
			assertThat(stored).isEqualTo(community);
			repository.searchCommunity(stored._id, testContext.succeeding(foundCommunity -> testContext.verify(() -> {

				assertThat(foundCommunity).isEqualTo(stored);
				testContext.completeNow();

			})));

		}).sendJson(community.toJsonObject(), testContext);

	}

	/**
	 * Verify that return error when try to update an undefined community.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#updateCommunity(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityThatIsNotDefined(WebClient client, VertxTestContext testContext) {

		final Community community = new CommunityTest().createModelExample(1);
		testRequest(client, HttpMethod.PUT, Communities.PATH + "/undefined-community-identifier").expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty();
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).sendJson(community.toJsonObject(), testContext);
	}

	/**
	 * Verify that return error when try to update with a model that is not a
	 * community.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#updateCommunity(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityWithANotCommunityObject(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storeCommunity(new CommunityTest().createModelExample(1), testContext.succeeding(community -> {

			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + community._id).expect(res -> {

				assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
				final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
				assertThat(error.code).isNotEmpty();
				assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
				testContext.completeNow();

			}).sendJson(new JsonObject().put("udefinedKey", "value"), testContext);
		}));
	}

	/**
	 * Verify that not update a community if any change is done.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#updateCommunity(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityBecauseNotChangesHasDone(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storeCommunity(new CommunityTest().createModelExample(1), testContext.succeeding(community -> {

			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + community._id).expect(res -> {

				assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
				final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
				assertThat(error.code).isNotEmpty();
				assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
				testContext.completeNow();

			}).sendJson(new JsonObject(), testContext);
		}));

	}

	/**
	 * Verify that not update a community because the source is not valid.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#updateCommunity(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityBecauseBadSource(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storeCommunity(new CommunityTest().createModelExample(1), testContext.succeeding(community -> {

			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + community._id).expect(res -> {

				assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
				final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
				assertThat(error.code).isNotEmpty().endsWith(".avatar");
				assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
				testContext.completeNow();

			}).sendJson(new JsonObject().put("avatar", "image.png"), testContext);
		}));

	}

	/**
	 * Verify that can update a complex community with another.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunity(CommunitiesRepository repository, WebClient client, VertxTestContext testContext) {

		final Community created = new CommunityTest().createModelExample(23);
		created.validate("codePrefix");

		repository.storeCommunity(created, testContext.succeeding(storedCommunity -> {

			final Community newCommunity = new CommunityTest().createModelExample(2);
			newCommunity._id = UUID.randomUUID().toString();
			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + storedCommunity._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
						final Community updated = assertThatBodyIs(Community.class, res);
						assertThat(updated).isNotEqualTo(storedCommunity).isNotEqualTo(newCommunity);
						newCommunity._id = storedCommunity._id;
						newCommunity.sinceTime = storedCommunity.sinceTime;
						assertThat(updated).isEqualTo(newCommunity);
						testContext.completeNow();

					})).sendJson(newCommunity.toJsonObject(), testContext);
		}));

	}

	/**
	 * Verify that return error when delete an undefined community.
	 *
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotDeleteCommunityWithAnUndefinedCommunityId(WebClient client, VertxTestContext testContext) {

		testRequest(client, HttpMethod.DELETE, Communities.PATH + "/undefined-community-identifier").expect(res -> {

			assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
			final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
			assertThat(error.code).isNotEmpty();
			assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
			testContext.completeNow();

		}).send(testContext);
	}

	/**
	 * Verify that can delete a community.
	 *
	 * @param repository  to access the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldDeleteCommunity(CommunitiesRepository repository, WebClient client, VertxTestContext testContext) {

		repository.storeCommunity(new Community(), testContext.succeeding(storedCommunity -> {

			testRequest(client, HttpMethod.DELETE, Communities.PATH + "/" + storedCommunity._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.NO_CONTENT.getStatusCode());
						testContext.completeNow();

					})).send(testContext);

		}));

	}

	/**
	 * Verify that can update only the community name.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunityName(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		final Community created = new CommunityTest().createModelExample(23);
		created.validate("codePrefix");
		repository.storeCommunity(created, testContext.succeeding(storedCommunity -> {

			final Community newCommunity = new Community();
			newCommunity._id = UUID.randomUUID().toString();
			newCommunity.name = "New community name";
			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + storedCommunity._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
						final Community updated = assertThatBodyIs(Community.class, res);
						assertThat(updated).isNotEqualTo(storedCommunity).isNotEqualTo(newCommunity);
						storedCommunity.name = "New community name";
						assertThat(updated).isEqualTo(storedCommunity);
						testContext.completeNow();

					})).sendJson(newCommunity.toJsonObject(), testContext);
		}));

	}

	/**
	 * Verify that can update only the community description.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunityDescription(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		final Community created = new CommunityTest().createModelExample(23);
		created.validate("codePrefix");
		repository.storeCommunity(created, testContext.succeeding(storedCommunity -> {

			final Community newCommunity = new Community();
			newCommunity._id = UUID.randomUUID().toString();
			newCommunity.description = "New community description";
			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + storedCommunity._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
						final Community updated = assertThatBodyIs(Community.class, res);
						assertThat(updated).isNotEqualTo(storedCommunity).isNotEqualTo(newCommunity);
						storedCommunity.description = "New community description";
						assertThat(updated).isEqualTo(storedCommunity);
						testContext.completeNow();

					})).sendJson(newCommunity.toJsonObject(), testContext);
		}));

	}

	/**
	 * Verify that can update the keywords of a community.
	 *
	 * @param repository  to access the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#updateCommunity(String, JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunityKeyword(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		final Community created = new CommunityTest().createModelExample(23);
		created.validate("codePrefix");
		repository.storeCommunity(created, testContext.succeeding(storedCommunity -> {

			final Community newCommunity = new Community();
			newCommunity.keywords = new ArrayList<>();
			newCommunity.keywords.add("    ");
			newCommunity.keywords.add("New keyword");
			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + storedCommunity._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
						final Community updated = assertThatBodyIs(Community.class, res);
						assertThat(updated).isNotEqualTo(storedCommunity).isNotEqualTo(newCommunity);

						storedCommunity.keywords.clear();
						storedCommunity.keywords.add("New keyword");
						assertThat(updated).isEqualTo(storedCommunity);

					})).sendJson(newCommunity.toJsonObject(), testContext);
		}));

	}

	/**
	 * Verify that can update only the community avatar.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#retrieveCommunity(String,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldUpdateCommunityAvatar(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		final Community created = new CommunityTest().createModelExample(23);
		created.validate("codePrefix");
		repository.storeCommunity(created, testContext.succeeding(storedCommunity -> {

			final Community newCommunity = new Community();
			newCommunity._id = UUID.randomUUID().toString();
			newCommunity.avatar = "http://host.com/newAvatar.png";
			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + storedCommunity._id)
					.expect(res -> testContext.verify(() -> {

						assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
						final Community updated = assertThatBodyIs(Community.class, res);
						assertThat(updated).isNotEqualTo(storedCommunity).isNotEqualTo(newCommunity);
						storedCommunity.avatar = "http://host.com/newAvatar.png";
						assertThat(updated).isEqualTo(storedCommunity);
						testContext.completeNow();

					})).sendJson(newCommunity.toJsonObject(), testContext);
		}));

	}

	/**
	 * Verify that not update a community since time.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#updateCommunity(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateCommunitySinceTime(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storeCommunity(new CommunityTest().createModelExample(1), testContext.succeeding(community -> {

			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + community._id).expect(res -> {

				assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
				final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
				assertThat(error.code).isNotEmpty().doesNotContain(".sinceTime");
				assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
				testContext.completeNow();

			}).sendJson(new JsonObject().put("sinceTime", 0l), testContext);
		}));

	}

	/**
	 * Verify that not update a community since time.
	 *
	 * @param repository  that manage the communities.
	 * @param client      to connect to the server.
	 * @param testContext context to test.
	 *
	 * @see Communities#updateCommunity(String, io.vertx.core.json.JsonObject,
	 *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
	 */
	@Test
	public void shouldNotUpdateCommunityId(CommunitiesRepository repository, WebClient client,
			VertxTestContext testContext) {

		repository.storeCommunity(new CommunityTest().createModelExample(1), testContext.succeeding(community -> {

			testRequest(client, HttpMethod.PUT, Communities.PATH + "/" + community._id).expect(res -> {

				assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
				final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
				assertThat(error.code).isNotEmpty().doesNotContain("._id");
				assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
				testContext.completeNow();

			}).sendJson(new JsonObject().put("_id", "Identifier"), testContext);
		}));

	}

}
