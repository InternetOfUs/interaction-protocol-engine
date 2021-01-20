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
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
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
import static io.reactiverse.junit5.web.TestRequest.queryParam;
import static io.reactiverse.junit5.web.TestRequest.testRequest;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.ErrorMessage;
import eu.internetofus.common.vertx.AbstractModelResourcesIT;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import io.vertx.core.Future;
import io.vertx.core.Vertx;
import io.vertx.core.http.HttpMethod;
import io.vertx.ext.web.client.WebClient;
import io.vertx.junit5.VertxTestContext;
import java.util.UUID;
import javax.ws.rs.core.Response.Status;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

/**
 * The integration test over the {@link Norms}.
 *
 * @see Norms
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class NormsIT extends AbstractModelResourcesIT<PublishedNorm, String> {

  /**
   * {@inheritDoc}
   */
  @Override
  protected String modelPath() {

    return Norms.PATH;
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected PublishedNorm createInvalidModel() {

    return new PublishedNormTest().createModelExample(1);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<PublishedNorm> createValidModelExample(final int index, final Vertx vertx,
      final VertxTestContext testContext) {

    return testContext.assertComplete(new PublishedNormTest().createModelExample(index, vertx, testContext));

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected Future<PublishedNorm> storeModel(final PublishedNorm source, final Vertx vertx,
      final VertxTestContext testContext) {

    return NormsRepository.createProxy(vertx).storePublishedNorm(source);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected void assertThatCreatedEquals(final PublishedNorm source, final PublishedNorm target) {

    source.id = target.id;
    source._creationTs = target._creationTs;
    source._lastUpdateTs = target._lastUpdateTs;
    source.norm.id = target.norm.id;
    assertThat(source).isEqualTo(target);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  protected String idOf(final PublishedNorm model) {

    return model.id;
  }

  /**
   * Verify that found some norms by its name.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String,
   *      Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByName(final Vertx vertx, final WebClient client, final VertxTestContext testContext) {

    final var publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final var name = UUID.randomUUID().toString();
    publishedNorm1.name = name + " 1";
    testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1))
        .onSuccess(storedPublishedNorm1 -> {

          final var publishedNorm2 = new PublishedNormTest().createModelExample(2);
          publishedNorm2.name += " " + name + " 2";
          testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2))
              .onSuccess(storedPublishedNorm2 -> {

                testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", "/.*" + name + ".*/"))
                    .expect(res -> {

                      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
                      final var page = assertThatBodyIs(PublishedNormsPage.class, res);
                      assertThat(page.offset).isEqualTo(0);
                      assertThat(page.total).isEqualTo(2);
                      assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1, storedPublishedNorm2);

                    }).send(testContext);
              });
        });
  }

  /**
   * Verify that found some norms by its description.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String,
   *      Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByDescription(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final var description = UUID.randomUUID().toString();
    publishedNorm1.description = description;
    testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1))
        .onSuccess(storedPublishedNorm1 -> {

          final var publishedNorm2 = new PublishedNormTest().createModelExample(2);
          publishedNorm2.description = description;
          testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2))
              .onSuccess(storedPublishedNorm2 -> {

                testRequest(client, HttpMethod.GET, Norms.PATH)
                    .with(queryParam("description", description), queryParam("offset", "1")).expect(res -> {

                      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
                      final var page = assertThatBodyIs(PublishedNormsPage.class, res);
                      assertThat(page.offset).isEqualTo(1);
                      assertThat(page.total).isEqualTo(2);
                      assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm2);

                    }).send(testContext);
              });
        });
  }

  /**
   * Verify that found some norms by a keyword.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String,
   *      Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByAKeyword(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final var keyword = UUID.randomUUID().toString();
    publishedNorm1.keywords.add(keyword);
    testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm1))
        .onSuccess(storedPublishedNorm1 -> {

          final var publishedNorm2 = new PublishedNormTest().createModelExample(2);
          publishedNorm2.keywords.add(keyword);
          testContext.assertComplete(NormsRepository.createProxy(vertx).storePublishedNorm(publishedNorm2))
              .onSuccess(storedPublishedNorm2 -> {

                testRequest(client, HttpMethod.GET, Norms.PATH)
                    .with(queryParam("keywords", keyword), queryParam("limit", "1")).expect(res -> {

                      assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
                      final var page = assertThatBodyIs(PublishedNormsPage.class, res);
                      assertThat(page.offset).isEqualTo(0);
                      assertThat(page.total).isEqualTo(2);
                      assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1);

                    }).send(testContext);
              });
        });
  }

  /**
   * Verify that found some norms by some keyword.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String,
   *      Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsBySomeKeyword(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final var keyword = UUID.randomUUID().toString();
    publishedNorm1.keywords.add(keyword);
    final var repository = NormsRepository.createProxy(vertx);
    testContext.assertComplete(repository.storePublishedNorm(publishedNorm1)).onSuccess(storedPublishedNorm1 -> {

      final var publishedNorm2 = new PublishedNormTest().createModelExample(2);
      publishedNorm2.keywords.add(1, keyword);
      testContext.assertComplete(repository.storePublishedNorm(publishedNorm2)).onSuccess(storedPublishedNorm2 -> {

        final var publishedNorm3 = new PublishedNormTest().createModelExample(30);
        publishedNorm3.keywords.add(0, keyword);
        testContext.assertComplete(repository.storePublishedNorm(publishedNorm3)).onSuccess(storedPublishedNorm3 -> {

          testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("keywords", keyword + ",keyword 1"))
              .expect(res -> {

                assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
                final var page = assertThatBodyIs(PublishedNormsPage.class, res);
                assertThat(page.offset).isEqualTo(0);
                assertThat(page.total).isEqualTo(2);
                assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1, storedPublishedNorm2);

              }).send(testContext);
        });
      });
    });
  }

  /**
   * Verify that found some norms by its publisherId.
   *
   * @param vertx       event bus to use.
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String,
   *      Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldFoundNormsByPublisherId(final Vertx vertx, final WebClient client,
      final VertxTestContext testContext) {

    final var publishedNorm1 = new PublishedNormTest().createModelExample(1);
    final var publisherId = "http://host.com/publisherId_" + UUID.randomUUID().toString() + ".png";
    publishedNorm1.publisherId = publisherId;
    final var repository = NormsRepository.createProxy(vertx);
    testContext.assertComplete(repository.storePublishedNorm(publishedNorm1)).onSuccess(storedPublishedNorm1 -> {

      final var publishedNorm2 = new PublishedNormTest().createModelExample(2);
      publishedNorm2.publisherId = publisherId;
      testContext.assertComplete(repository.storePublishedNorm(publishedNorm2)).onSuccess(storedPublishedNorm2 -> {

        testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("publisherId", publisherId)).expect(res -> {

          assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
          final var page = assertThatBodyIs(PublishedNormsPage.class, res);
          assertThat(page.offset).isEqualTo(0);
          assertThat(page.total).isEqualTo(2);
          assertThat(page.norms).isNotEmpty().containsExactly(storedPublishedNorm1, storedPublishedNorm2);

        }).send(testContext);
      });
    });
  }

  /**
   * Verify that found a published norm.
   *
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String,
   *      Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldNotFoundNormsBecausePatternIsNotValid(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", "/a{12(/")).expect(res -> {

      assertThat(res.statusCode()).isEqualTo(Status.BAD_REQUEST.getStatusCode());
      final var error = assertThatBodyIs(ErrorMessage.class, res);
      assertThat(error.code).isNotEmpty();
      assertThat(error.message).isNotEmpty().isNotEqualTo(error.code);

    }).send(testContext);
  }

  /**
   * Verify that return an empty published norms if any match.
   *
   * @param client      to connect to the server.
   * @param testContext context to test.
   *
   * @see Norms#retrievePublishedNormsPage(String, String, java.util.List, String,
   *      Long, Long, java.util.List, int, int,
   *      io.vertx.ext.web.api.service.ServiceRequest, io.vertx.core.Handler)
   */
  @Test
  public void shouldEmptyPageIfAnyPublishedNormMatch(final WebClient client, final VertxTestContext testContext) {

    testRequest(client, HttpMethod.GET, Norms.PATH).with(queryParam("name", UUID.randomUUID().toString()))
        .expect(res -> {

          assertThat(res.statusCode()).isEqualTo(Status.OK.getStatusCode());
          final var page = assertThatBodyIs(PublishedNormsPage.class, res);
          assertThat(page.offset).isEqualTo(0);
          assertThat(page.total).isEqualTo(0);
          assertThat(page.norms).isNull();

        }).send(testContext);
  }

}
