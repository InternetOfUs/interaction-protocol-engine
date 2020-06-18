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

import static eu.internetofus.common.vertx.HttpResponses.assertThatBodyIs;
import static io.vertx.junit5.web.TestRequest.queryParam;
import static io.vertx.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.UUID;

import javax.ws.rs.core.Response.Status;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.components.ErrorMessage;
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
   * @see Norms#retrievePublishedNorm(String, io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundPublishedNormWithAnUndefinedPublishedNormId(final WebClient client, final VertxTestContext testContext) {

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
   * @see Norms#retrievePublishedNorm(String, io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundPublishedNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNormTest().createModelExample(1), testContext.succeeding(stored -> {

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
   * @see Norms#publishNorm(io.vertx.core.json.JsonObject, io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotStoreANonPublishedNormObject(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty().isEqualTo("bad_publishedNorm");
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
      testContext.completeNow();

    }).sendJson(new JsonObject().put("udefinedKey", "value"), testContext);
  }

  /**
   * Verify that can not publish an invalid norm.
   *
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#publishNorm(io.vertx.core.json.JsonObject, io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotPublishInvalidNorm(final WebClient client, final VertxTestContext testContext) {

    final PublishedNorm norm = new PublishedNormTest().createModelExample(1);
    testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty().isEqualTo("bad_publishedNorm.publisherId");
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
      testContext.completeNow();

    }).sendJson(norm.toJsonObject(), testContext);
  }

  /**
   * Verify that can publish a norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#publishNorm(io.vertx.core.json.JsonObject, io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldPublishNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    new PublishedNormTest().createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      model._creationTs = 0;
      model._lastUpdateTs = 1;
      final long now = TimeManager.now();
      testRequest(client, HttpMethod.POST, Norms.PATH).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.CREATED.getStatusCode());
        final PublishedNorm published = assertThatBodyIs(PublishedNorm.class, res);
        assertThat(published).isNotNull();
        assertThat(published._creationTs).isGreaterThanOrEqualTo(now);
        assertThat(published._lastUpdateTs).isGreaterThanOrEqualTo(published._creationTs);
        model._creationTs = published._creationTs;
        model._lastUpdateTs = published._lastUpdateTs;
        model.id = published.id;
        model.norm.id = published.norm.id;
        assertThat(published).isEqualTo(model);
        testContext.completeNow();

      }).sendJson(model.toJsonObject(), testContext);

    }));
  }

  /**
   * Verify that found some norms by its name.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByName(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final String name = UUID.randomUUID().toString();
    publishedNorm1.name = name + " 1";
    NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

      final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
      publishedNorm2.name += " " + name + " 2";
      NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

        testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", "/.*" + name + ".*/")).expect(res -> {

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
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByDescription(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final String description = UUID.randomUUID().toString();
    publishedNorm1.description = description;
    NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

      final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
      publishedNorm2.description = description;
      NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

        testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("description", description), queryParam("offset", "1")).expect(res -> {

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
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByAKeyword(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final PublishedNorm publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final String keyword = UUID.randomUUID().toString();
    publishedNorm1.keywords.add(keyword);
    NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1, testContext.succeeding(storedPublishedNorm1 -> {

      final PublishedNorm publishedNorm2 = new PublishedNormTest().createModelExample(2);
      publishedNorm2.keywords.add(keyword);
      NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2, testContext.succeeding(storedPublishedNorm2 -> {

        testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("keywords", keyword), queryParam("limit", "1")).expect(res -> {

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
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsBySomeKeyword(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

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

          testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("keywords", keyword + ",keyword 1")).expect(res -> {

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
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByPublisherId(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

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
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundNormsBecausePatternIsNotValid(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", "/a{12(/")).expect(res -> {

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
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldEmptyPageIfAnyPublishedNormMatch(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", UUID.randomUUID().toString())).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
      final PublishedNormsPage page = assertThatBodyIs(PublishedNormsPage.class, res);
      assertThat(page.offset).isEqualTo(0);
      assertThat(page.total).isEqualTo(0);
      assertThat(page.norms).isNull();
      testContext.completeNow();

    }).send(testContext);
  }

  /**
   * Verify that not delete an undefined norm.
   *
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotDeleteUndefinedNorm(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.DELETE, Norms.PATH + "/undefined").expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
      final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty();
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
      testContext.completeNow();

    }).send(testContext);
  }

  /**
   * Verify that delete a norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldDeleteNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(norm -> {

      testRequest(client, HttpMethod.DELETE, Norms.PATH + "/" + norm.id).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.NO_CONTENT.getStatusCode());
        testContext.completeNow();

      }).send(testContext);

    }));
  }

  /**
   * Verify that not update with a bad norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdateWithBadNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(norm -> {
      testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + norm.id).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
        final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
        assertThat(error.code).isNotEmpty();
        assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
        testContext.completeNow();

      }).sendJson(new JsonObject().put("key", "value"), testContext);

    }));
  }

  /**
   * Verify that not update with an invalid norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdateWithInvalidNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(norm -> {
      testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + norm.id).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
        final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
        assertThat(error.code).isNotEmpty();
        assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
        testContext.completeNow();

      }).sendJson(new PublishedNormTest().createModelExample(1).toJsonObject(), testContext);

    }));
  }

  /**
   * Verify that not update with an undefined norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdateWithUndefinedNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(norm -> {
      testRequest(client, HttpMethod.PUT, Norms.PATH + "/undefined").expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
        final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
        assertThat(error.code).isNotEmpty();
        assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
        testContext.completeNow();

      }).sendJson(new PublishedNormTest().createModelExample(1).toJsonObject(), testContext);

    }));
  }

  /**
   * Verify that not update if no changes are done.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotUpdateWithEmptyNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    new PublishedNormTest().createModelExample(1, vertx, testContext, testContext.succeeding(created -> {

      NormsRepository.createProxy(vertx).storePublishedNorm(created, testContext.succeeding(norm -> {
        testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + norm.id).expect(res -> {

          assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
          final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
          assertThat(error.code).isNotEmpty();
          assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
          testContext.completeNow();

        }).sendJson(new PublishedNorm().toJsonObject(), testContext);

      }));
    }));
  }

  /**
   * Verify that update a norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldUpdateNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    new PublishedNormTest().createModelExample(1, vertx, testContext, testContext.succeeding(created -> {

      new PublishedNormTest().createModelExample(2, vertx, testContext, testContext.succeeding(source -> {
        NormsRepository.createProxy(vertx).storePublishedNorm(created, testContext.succeeding(target -> {

          final long now = TimeManager.now();
          testRequest(client, HttpMethod.PUT, Norms.PATH + "/" + target.id).expect(res -> {

            assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
            final PublishedNorm updated = assertThatBodyIs(PublishedNorm.class, res);
            assertThat(updated).isNotNull();
            assertThat(updated._lastUpdateTs).isGreaterThanOrEqualTo(now);
            source._creationTs = target._creationTs;
            source._lastUpdateTs = updated._lastUpdateTs;
            source.id = target.id;
            source.norm.id = target.norm.id;
            assertThat(updated).isEqualTo(source);
            testContext.completeNow();

          }).sendJson(source.toJsonObject(), testContext);

        }));
      }));
    }));
  }

  /**
   * Verify that not merge with a bad norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotMergeWithBadNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(norm -> {
      testRequest(client, HttpMethod.PATCH, Norms.PATH + "/" + norm.id).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
        final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
        assertThat(error.code).isNotEmpty();
        assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
        testContext.completeNow();

      }).sendJson(new JsonObject().put("key", "value"), testContext);

    }));
  }

  /**
   * Verify that not merge with an invalid norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotMergeWithInvalidNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(norm -> {
      testRequest(client, HttpMethod.PATCH, Norms.PATH + "/" + norm.id).expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
        final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
        assertThat(error.code).isNotEmpty();
        assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
        testContext.completeNow();

      }).sendJson(new PublishedNormTest().createModelExample(1).toJsonObject(), testContext);

    }));
  }

  /**
   * Verify that not merge with an undefined norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotMergeWithUndefinedNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    NormsRepository.createProxy(vertx).storePublishedNorm(new PublishedNorm(), testContext.succeeding(norm -> {
      testRequest(client, HttpMethod.PATCH, Norms.PATH + "/undefined").expect(res -> {

        assertThat(res.statusCode()).isEqualTo(Status.NOT_FOUND.getStatusCode());
        final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
        assertThat(error.code).isNotEmpty();
        assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
        testContext.completeNow();

      }).sendJson(new PublishedNormTest().createModelExample(1).toJsonObject(), testContext);

    }));
  }

  /**
   * Verify that not merge if no changes are done.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotMergeWithEmptyNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    new PublishedNormTest().createModelExample(1, vertx, testContext, testContext.succeeding(created -> {

      NormsRepository.createProxy(vertx).storePublishedNorm(created, testContext.succeeding(norm -> {
        testRequest(client, HttpMethod.PATCH, Norms.PATH + "/" + norm.id).expect(res -> {

          assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
          final ErrorMessage error = assertThatBodyIs(ErrorMessage.class, res);
          assertThat(error.code).isNotEmpty();
          assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);
          testContext.completeNow();

        }).sendJson(new PublishedNorm().toJsonObject(), testContext);

      }));
    }));
  }

  /**
   * Verify that merge a norm.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String, Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.OperationRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldMergeNorm(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    new PublishedNormTest().createModelExample(1, vertx, testContext, testContext.succeeding(created -> {

      NormsRepository.createProxy(vertx).storePublishedNorm(created, testContext.succeeding(target -> {

        final PublishedNorm source = new PublishedNorm();
        source.name = "NEW NAME";
        final long now = TimeManager.now();
        testRequest(client, HttpMethod.PATCH, Norms.PATH + "/" + target.id).expect(res -> {

          assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
          final PublishedNorm merged = assertThatBodyIs(PublishedNorm.class, res);
          assertThat(merged).isNotNull();
          assertThat(merged._lastUpdateTs).isGreaterThanOrEqualTo(now);
          target._lastUpdateTs = merged._lastUpdateTs;
          target.name = "NEW NAME";
          assertThat(merged).isEqualTo(target);
          testContext.completeNow();

        }).sendJson(source.toJsonObject(), testContext);

      }));
    }));
  }

}
